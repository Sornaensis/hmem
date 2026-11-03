"""Rebuild static HTML using HTML5/CSS parsers and typed, finite allowlists."""
import base64
import html
import math
import re
from PIL import ImageColor
from selectolax.lexbor import LexborHTMLParser
import cssselect2
import tinycss2
from .images import render_image
from .policy import PreviewError, require

TAGS = set("div span p h1 h2 h3 h4 h5 h6 section article header footer main aside nav address blockquote pre code kbd samp var b strong i em u s small sub sup mark abbr cite q br hr wbr ul ol li dl dt dd table caption colgroup col thead tbody tfoot tr th td figure figcaption img details summary time data ruby rt rp".split())
CONVERT = {"a": "span", "button": "span", "form": "div", "input": "span", "textarea": "pre",
           "select": "span", "option": "span", "label": "span", "fieldset": "div", "legend": "span",
           "details": "div", "summary": "strong"}
DROP = set("script noscript iframe frame frameset base link meta object embed template svg math audio video source track canvas portal".split())
VOID = {"br", "hr", "wbr", "img", "col"}
NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_-]{0,63}\Z")
CUSTOM = re.compile(r"--[A-Za-z_][A-Za-z0-9_-]{0,61}\Z")
LENGTH = set("width height min-width min-height max-width max-height top right bottom left margin margin-top margin-right margin-bottom margin-left padding padding-top padding-right padding-bottom padding-left gap row-gap column-gap border-radius border-top-left-radius border-top-right-radius border-bottom-left-radius border-bottom-right-radius border-width border-top-width border-right-width border-bottom-width border-left-width outline-width outline-offset border-spacing font-size letter-spacing word-spacing text-indent flex-basis".split())
COLORS = set("color background-color border-color border-top-color border-right-color border-bottom-color border-left-color outline-color text-decoration-color".split())
ENUMS = {
    "display": "none block inline inline-block flex inline-flex grid inline-grid table table-row table-cell list-item flow-root",
    "position": "static relative absolute", "box-sizing": "border-box content-box",
    "overflow": "visible hidden clip auto scroll", "overflow-x": "visible hidden clip auto scroll",
    "overflow-y": "visible hidden clip auto scroll", "float": "none left right", "clear": "none left right both",
    "font-style": "normal italic oblique", "font-weight": "normal bold bolder lighter",
    "text-align": "left right center justify start end", "text-transform": "none uppercase lowercase capitalize",
    "white-space": "normal nowrap pre pre-wrap pre-line break-spaces",
    "word-break": "normal break-all keep-all break-word", "overflow-wrap": "normal break-word anywhere",
    "text-decoration": "none underline overline line-through", "text-decoration-line": "none underline overline line-through",
    "vertical-align": "baseline middle top bottom text-top text-bottom sub super",
    "list-style-type": "none disc circle square decimal lower-alpha upper-alpha lower-roman upper-roman",
    "list-style-position": "inside outside", "border-collapse": "collapse separate",
    "border-style": "none hidden solid dotted dashed double groove ridge inset outset",
    "flex-direction": "row row-reverse column column-reverse", "flex-wrap": "nowrap wrap wrap-reverse",
    "justify-content": "normal start end flex-start flex-end center space-between space-around space-evenly",
    "align-content": "normal start end flex-start flex-end center stretch space-between space-around space-evenly",
    "align-items": "normal start end flex-start flex-end center stretch baseline",
    "align-self": "auto normal start end flex-start flex-end center stretch baseline",
    "justify-items": "normal start end center stretch", "justify-self": "auto normal start end center stretch",
    "grid-auto-flow": "row column dense", "table-layout": "auto fixed",
}
FONTS = {"serif", "sans-serif", "monospace", "system-ui", "cursive", "fantasy", "arial", "helvetica",
         "times new roman", "georgia", "segoe ui", "courier new"}
UNITS = {"px": 4096, "em": 256, "rem": 256, "vh": 100, "vw": 100, "vmin": 100, "vmax": 100,
         "pt": 3072, "pc": 256, "cm": 100, "mm": 1000, "in": 40, "ch": 512, "ex": 512}


def clean_tokens(tokens):
    return [x for x in tokens if x.type not in {"whitespace", "comment"}]


def substitute(tokens, variables, active=(), depth=0):
    if depth > 16 or len(tokens) > 256:
        return None
    result = []
    for token in tokens:
        if token.type == "function":
            if token.lower_name != "var":
                # Typed functions are checked below, with nested vars already expanded.
                inner = substitute(token.arguments, variables, active, depth + 1)
                if inner is None:
                    return None
                token = tinycss2.ast.FunctionBlock(token.source_line, token.source_column, token.name, inner)
                result.append(token)
                continue
            args = clean_tokens(token.arguments)
            if not args or args[0].type != "ident" or not CUSTOM.fullmatch(args[0].value):
                return None
            name = args[0].value
            if name in active:
                return None
            if len(args) > 1 and not (args[1].type == "literal" and args[1].value == ","):
                return None
            source = variables.get(name, args[2:] if len(args) > 2 else None)
            if source is None:
                return None
            replacement = substitute(source, variables, active + (name,), depth + 1)
            if replacement is None:
                return None
            result.extend(replacement)
        elif token.type in {"error", "url", "{} block", "[] block", "() block", "at-keyword"}:
            return None
        else:
            result.append(token)
        if len(result) > 256:
            return None
    return result


def length(token, negative=False, fr=False):
    if token.type == "ident":
        return token.lower_value in {"auto", "normal", "min-content", "max-content", "none"}
    if token.type not in {"number", "dimension", "percentage"}:
        return False
    value = token.value
    if not math.isfinite(value) or value < ( -4096 if negative else 0):
        return False
    if token.type == "number":
        return value == 0
    maximum = 1000 if token.type == "percentage" else UNITS.get(token.lower_unit)
    if fr and token.type == "dimension" and token.lower_unit == "fr":
        maximum = 100
    return maximum is not None and abs(value) <= maximum


def color(tokens):
    if len(tokens) != 1:
        return False
    token = tokens[0]
    if token.type == "ident" and token.lower_value in {"transparent", "currentcolor"}:
        return True
    if token.type in {"hash", "ident"}:
        try:
            ImageColor.getrgb(tinycss2.serialize(tokens))
            return True
        except ValueError:
            return False
    if token.type != "function" or token.lower_name not in {"rgb", "rgba", "hsl", "hsla"}:
        return False
    numbers = []
    for value in clean_tokens(token.arguments):
        if value.type == "literal" and value.value in {",", "/"}:
            continue
        if value.type not in {"number", "percentage"} or not math.isfinite(value.value):
            return False
        numbers.append(value)
    if not 3 <= len(numbers) <= 4:
        return False
    for index, value in enumerate(numbers):
        maximum = 100 if value.type == "percentage" else (1 if index == 3 else
                    (360 if index == 0 and token.lower_name.startswith("hsl") else 255))
        if not 0 <= value.value <= maximum:
            return False
    return True


def track_count(tokens, depth=0):
    if depth > 3 or not 1 <= len(tokens) <= 64:
        return None
    count = 0
    for token in tokens:
        if length(token, fr=True):
            count += 1
        elif token.type == "function" and token.lower_name in {"repeat", "minmax"}:
            args = clean_tokens(token.arguments)
            if token.lower_name == "repeat":
                if len(args) < 3 or args[0].type != "number" or not args[0].is_integer or not 1 <= args[0].value <= 32:
                    return None
                nested = track_count(args[2:], depth + 1)
                if args[1].type != "literal" or args[1].value != "," or nested is None:
                    return None
                count += int(args[0].value) * nested
            else:
                if len(args) != 3 or args[1].type != "literal" or args[1].value != "," or not all(length(t, fr=True) for t in (args[0], args[2])):
                    return None
                count += 1
        else:
            return None
    return count if count <= 64 else None


def typed_value(prop, tokens):
    tokens = clean_tokens(tokens)
    if not tokens or len(tokens) > 64:
        return False
    if prop in COLORS or prop == "background":
        return color(tokens)
    if prop in LENGTH:
        return len(tokens) <= 4 and all(length(x, negative=prop.startswith("margin") or prop in {"top", "right", "bottom", "left", "letter-spacing", "text-indent"}) for x in tokens)
    if prop in ENUMS and all(x.type == "ident" for x in tokens):
        return len(tokens) <= 4 and all(x.type == "ident" and x.lower_value in ENUMS[prop].split() for x in tokens)
    if prop in {"opacity", "flex-grow", "flex-shrink", "line-height", "font-weight", "z-index", "order"}:
        maximum = {"opacity": 1, "flex-grow": 100, "flex-shrink": 100, "line-height": 10,
                   "font-weight": 900, "z-index": 10, "order": 100}[prop]
        return len(tokens) == 1 and (tokens[0].type == "number" and math.isfinite(tokens[0].value)
               and (-maximum if prop in {"z-index", "order"} else 0) <= tokens[0].value <= maximum
               or prop == "line-height" and length(tokens[0]))
    if prop == "font-family":
        groups, group = [], []
        for token in tokens:
            if token.type == "literal" and token.value == ",":
                groups.append(" ".join(group).lower()); group = []
            elif token.type in {"ident", "string"}:
                group.append(token.value)
            else:
                return False
        groups.append(" ".join(group).lower())
        return len(groups) <= 8 and all(x in FONTS for x in groups)
    if prop in {"border", "border-top", "border-right", "border-bottom", "border-left", "outline"}:
        return len(tokens) <= 3 and all(length(x) or color([x]) or
                x.type == "ident" and x.lower_value in ENUMS["border-style"].split() for x in tokens)
    if prop == "flex":
        return len(tokens) <= 3 and all(length(x) or x.type == "number" and 0 <= x.value <= 100
                    or x.type == "ident" and x.lower_value in {"none", "auto", "initial"} for x in tokens)
    if prop in {"grid-template-columns", "grid-template-rows", "grid-auto-columns", "grid-auto-rows"}:
        return track_count(tokens) is not None
    if prop in {"grid-column", "grid-row", "grid-column-start", "grid-column-end", "grid-row-start", "grid-row-end"}:
        return len(tokens) <= 5 and all(x.type == "ident" and x.lower_value in {"auto", "span"}
               or x.type == "number" and x.is_integer and 1 <= x.value <= 64
               or x.type == "literal" and x.value == "/" for x in tokens)
    return False


def declarations(content, globals_=None, custom_only=False):
    parsed = tinycss2.parse_declaration_list(content, skip_whitespace=True, skip_comments=True)
    require(len(parsed) <= 256, "css_declaration_limit")
    variables = dict(globals_ or {})
    for item in parsed:
        if item.type == "declaration" and CUSTOM.fullmatch(item.name) and len(item.value) <= 256:
            variables[item.name] = item.value
    require(len(variables) <= 128, "css_variable_limit")
    if custom_only:
        return variables
    result = []
    for item in parsed:
        if item.type != "declaration" or item.important or item.name.startswith("--"):
            continue
        prop = item.lower_name
        values = substitute(item.value, variables)
        if values is not None and typed_value(prop, values):
            value = tinycss2.serialize(values).strip().replace("<", "\\3c ").replace(">", "\\3e ")
            result.append(prop + ":" + value)
    return ";".join(result)


def selector(tokens):
    text = tinycss2.serialize(tokens).strip()
    if not 0 < len(text) <= 256 or text.count(",") >= 16:
        return None
    compact = clean_tokens(tokens)
    if len(compact) > 64:
        return None
    after = None
    for token in compact:
        if token.type == "ident":
            if after == ":":
                if token.lower_value not in {"root", "first-child", "last-child", "only-child", "empty"}:
                    return None
            elif after != "." and token.lower_value not in TAGS | {"html", "body"}:
                return None
            if not NAME.fullmatch(token.value):
                return None
        elif token.type == "hash":
            if not NAME.fullmatch(token.value):
                return None
        elif token.type == "literal":
            if token.value not in {".", ":", ">", "+", "~", "*", ","}:
                return None
        else:
            return None
        after = token.value if token.type == "literal" else None
    try:
        cssselect2.compile_selector_list(text)
    except cssselect2.SelectorError:
        return None
    return text


def sanitize(raw, limits, metadata=None):
    require(len(raw) <= limits.html_bytes, "html_input_limit")
    source = raw.decode("utf-8", errors="strict")
    tree = LexborHTMLParser(source, scripting=False)
    stack = [(tree.root, 0)]
    nodes, styles = 0, []
    while stack:
        node, depth = stack.pop()
        nodes += 1
        require(nodes <= limits.dom_nodes and depth <= limits.dom_depth, "html_structure_limit")
        if node.tag == "style":
            styles.append(node.text())
        child = node.child
        children = []
        while child is not None:
            children.append(child)
            child = child.next
        stack.extend((child, depth + 1) for child in reversed(children))
    rules, total_rules, variables = [], 0, {}
    for style in styles:
        parsed = tinycss2.parse_stylesheet(style, skip_comments=True, skip_whitespace=True)
        total_rules += len(parsed)
        require(total_rules <= limits.css_rules, "css_rule_limit")
        for rule in parsed:
            if rule.type == "qualified-rule":
                selected = selector(rule.prelude)
                if selected:
                    rules.append((selected, rule.content))
                    if selected == ":root":
                        variables.update(declarations(rule.content, custom_only=True))
    css = []
    for selected, content in rules:
        safe = declarations(content, variables)
        if safe:
            css.append(selected + "{" + safe + "}")
    image_work, image_count = {"pixels": 0}, [0]

    def attributes(node):
        result = []
        for key, value in node.attributes.items():
            value = value or ""
            if key in {"title", "aria-label", "alt"}:
                value = value[:1024]
            elif key == "id":
                if not NAME.fullmatch(value): continue
            elif key == "class":
                value = " ".join(x for x in value.split()[:32] if NAME.fullmatch(x))
            elif key == "style":
                value = declarations(value, variables)
                if not value: continue
            elif key == "lang":
                if re.fullmatch(r"[A-Za-z-]{1,35}", value) is None: continue
            elif key == "dir":
                if value not in {"ltr", "rtl", "auto"}: continue
            elif key in {"colspan", "rowspan", "width", "height"}:
                if not value.isascii() or not value.isdigit() or not 1 <= int(value) <= 4096: continue
            else:
                continue
            result.append(" " + key + '="' + html.escape(value, quote=True) + '"')
        return "".join(result)

    def rebuild(node):
        tag = node.tag
        if tag == "-text":
            return html.escape(node.text(), quote=False)
        if tag in DROP or tag in {"style", "head", "-comment"}:
            return ""
        if tag not in TAGS and tag not in CONVERT and tag not in {"html", "body"}:
            return ""
        converted = CONVERT.get(tag, tag)
        attrs = attributes(node)
        if tag == "img":
            image_count[0] += 1
            require(image_count[0] <= limits.embedded_images, "embedded_image_limit")
            src = node.attributes.get("src", "") or ""
            match = re.fullmatch(r"data:(image/(?:png|jpeg|webp));base64,([A-Za-z0-9+/=\r\n]+)", src)
            if match:
                try:
                    payload = base64.b64decode(match[2].replace("\r", "").replace("\n", ""), validate=True)
                    require(len(payload) <= limits.original_bytes, "embedded_image_limit")
                    png, _, animated = render_image(payload, match[1], limits, image_work)
                except PreviewError as error:
                    if error.code != "image_type_mismatch":
                        raise
                except (ValueError, OSError):
                    pass
                else:
                    image = '<img' + attrs + ' src="data:image/png;base64,' + base64.b64encode(png).decode("ascii") + '">'
                    if animated:
                        if metadata is not None:
                            metadata["animated_first_frame"] = True
                        image = ('<figure style="display:block!important">' + image +
                                 '<figcaption style="display:block!important">Animated image — first frame only</figcaption></figure>')
                    return image
            return '<span' + attrs + '>Image unavailable</span>'
        pieces = []
        child = node.child
        while child is not None:
            pieces.append(rebuild(child))
            child = child.next
        content = "".join(pieces)
        if tag == "input":
            content = html.escape((node.attributes.get("value") or node.attributes.get("placeholder") or "")[:1024])
        if tag in {"html", "body"}:
            return content if tag == "html" else "<body" + attrs + ">" + content + "</body>"
        if converted in VOID:
            return "<" + converted + attrs + ">"
        return "<" + converted + attrs + ">" + content + "</" + converted + ">"

    body = rebuild(tree.body) if tree.body else "<body></body>"
    safe_html = ('<!doctype html><html><head><meta charset="utf-8"><style>' + "".join(css) +
                 "</style></head>" + body + "</html>").encode("utf-8")
    require(len(safe_html) <= limits.output_bytes, "output_limit")
    return safe_html, image_work["pixels"]
