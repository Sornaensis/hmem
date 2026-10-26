# UI composition reference

The six accepted directions define the shared layout. The original HTML and reopened review are immutable inputs; simplified mock controls and example records do not replace production capabilities.

## Inputs and selected stages

Original SHA-256: `e4fb5d4475170c64af5a0680086d41a229acd5d0896cf946043e153bad40f2d9`.
Reopened SHA-256: `0e11b17c944322b6e9552dbaff97c979e1bbca37295d08fec93e422fddbccea6`.

| Area | Exact stage inside the reopened artifact |
|---|---|
| W, workspace | `#hm-workspace [data-variant="Tree with inspector"] > .hm` |
| C, cards | `#hm-cards [data-variant="Explicit sections"] > .hm` |
| B, persistent bar | `#hm-bar [data-variant="Persistent context"] > .hm` |
| A, audit | `#hm-audit [data-variant="Ledger and detail"] > .hm` |
| D, context dock | `#hm-dock [data-variant="Context dock"] > .hm` |
| V, conversation | `#hm-viewer [data-variant="Conversation and inspector"] > .hm` |

Select the corresponding `#hm-AREA-tab`, assert the unique visible selected stage and its direction, and exclude review/carousel chrome. Measure inner window/card rectangles separately from decorative component-stage padding. Do not scale bitmaps. Dock selections use scoped `[data-agent]`; viewer selections use `[data-view-session]` or `[data-hierarchy-session]`, with selected ID/state assertions.

## Geometry and identity

| Composition | Normal-content geometry |
|---|---|
| Workspace | 146px desktop rail, fluid main, 260px inspector; at <=850px rail120px and inspector below main; at <=650px one column and wrapping horizontal rail. |
| Sections | 14px/1.5 base, 18px/1.4 primary heading, 14px secondary heading, 12px metadata and 11px labels;14px spacing,8px radius;10x14px entity header,12x14px footer,12px card separation. |
| Persistent context | Global header minimum54px with12x16px padding; workspace controls12x16px/gap12px;tabs11x8px and local tabs9x8px;main padding20px,14px at<=650px. |
| Ledger | Fluid ledger plus270px detail,235px at<=850px;single column<=650px;timestamp/content110px+fluid with14px gap,narrow85px+fluid with8px gap. |
| Dock | `minmax(0,1fr)` Work and active-agent hierarchy on the left plus `minmax(300px,1.1fr)` conversation on the right; stack when the workspace tree-content pane is <=650px. Inline-size containment is on that pane: at a 900px viewport the 440px pane contains a narrower nested card, so the dock stacks without removing the workspace inspector. No block-size containment or viewer ownership change. |
| Conversation | 210px hierarchy/fluid conversation/230px inspector;<=850px hierarchy180px and inspector below conversation;<=650px single column. |

Tolerances are +/-2 CSSpx spacing, +/-4px fixed panes and +/-3 percentage points dock proportion on matching inputs. They do not excuse missing panes, wrong ordering, lost controls or premature1200px inspector stacking. Header heights are minima: long labels and200% text must wrap. Fine-pointer controls target32px and coarse-pointer controls44px;existing card disclosures retain their coordinated24px footprint/placement.

Preserve existing `style.css` canvas/surface/text/accent and semantic status/priority colors. Baseline production has a dark palette and no light palette or theme switch. Both OS color preferences therefore retain that palette; the mock palette difference is intentional. Preserve `/hmem-logo.svg` aspect ratio and current workspace/entity/status/auth/action icons;scale the logo in the shared global row. Keep independent tree triangles, lifecycle versus freshness, unknowns, authoritative counts and P1-P10 higher-number-first.

Production has more real controls than the examples: count/retry states, workspace details, separate all-entity and hierarchy search, permissions and authentication remain accessible. They wrap rather than being removed to force a mock height. The global row reuses the existing principal/mode/sign-out controls. Full-width workspace identity/search, tabs and real filter controls precede the rail/main/inspector row. The extra real hierarchy-filter row is a measured adaptation; it does not replace or hide the accepted tab row. Native main/sidebar overflow and measured context obstruction remain authoritative.

## Surface ownership

| Family | Current surfaces and preserved workflows |
|---|---|
| Shell/Home | AppShell,Groups,Page.Home,Page.Workspace;grouped workspace picker,identity/totals/tabs,bootstrap/required/failed auth,connection/toasts,NotFound. |
| Projects | Cards,CardSections,Editing,Dependencies,Move,DragDrop;root/nested/focused hierarchy,inspector,readiness,next tasks,Work/Agents/Evidence/History,all create/edit/status/priority/dependency/move/drop/delete confirmations. |
| Sessions | Sessions and viewer bridges;actual lineage/truncation,tree/list/detail/dock/inspector,live/quiet/terminated,runtime versus creator,raw/tool/save/integrity/encrypted/unsupported states. |
| Observations | Subject Tree/All/Files/query/match,selected/detached detail,drafts,editing/subjects/provenance/history/curation and confirmations. |
| Search/Timeline | Global/workspace/focused filters,loading/error/results,jump/return/history;ranges/granularity/chart/series/tooltips/events. |
| Audit/admin/dialogs | Global/workspace ledger,entity history,detail/diff/Revert/refusals;roles/memberships/purge/delete;shared modal/popover/focus restoration. |

Top-level routes are `/`,`/workspace/:id`,`/audit` and NotFound. Workspace sections are Projects,Sessions,Observations,Timeline,Audit and Administration. Memory.elm has no production callers;do not expose a new route. Chevron renderer work remains separately scoped (four renderers,24px,current placement/accessibility);Indigo Capsule scrollbar appearance remains separate.

Preserve bounded fetching/mounting/pins,authority generations,selected Observation occurrence/draft protection,physical anchors,keyboard/focus/follow/reading continuity including>2MiB prefix eviction. Session parent/ancestor context is separate from linked tasks and result membership;retain16-ancestor truncation and clear stale tool payloads on session/authority/audience change. Do not add capabilities or rewrite synchronization.

## Deterministic qualification matrix

Reference and production Main cases use identical record IDs,strings,timestamps,counts,statuses,selected task/session/tool,expansion,filters,sorting,authority and scroll anchor. Comparison copies change data only and record source/data hashes. Capture browser/fonts/device scale,settled font/paint,stage rectangles and source/asset/input hashes. Surfaces absent from the reference receive candidate-only evidence and compare shared primitives;do not claim nonexistent literal reference screens.

| Case IDs | Coverage | Budget |
|---|---|---|
| `B-{W,C,B,A,D,V}-{d,n,c,m}-{light,dark}` | Six selected stages;d1440x900,n900x900,c850x900,m320x900;both OS preferences. |48 reference/candidate pairs,96PNG. |
| `Z-{W,C,B,A,D,V}-{light,dark}` | Verified200% browser/text enlargement at1440x900 physical viewport;record method,resulting CSS viewport/computed font and control reflow. |12 pairs,24PNG. |
| `G-{W,C,B,A,D,V}-{650,651,850,851}` | Relevant breakpoint geometry on both reference/candidate. |24 assertion cases,0PNG. |
| `E-{OT,OA,OF,GS,TL,HM,AD,AU,DL}-{d,m}-{light,dark}` | Observation SubjectTree/All/Files,global/focused search+return,Timeline,Home/groups,admin,auth,dialogs/popovers;each named-family controls/substeps. |36 candidate-onlyPNG. |
| `S-{P,S,O,Q,H,A}-{L,E,R,U,P,K}` | Projects+mutation dialogs,Sessions+dock,Observation modes,search+Timeline,Home+auth+admin,global/workspace Audit+diff/Revert;320x900/dark. |36 candidate-onlyPNG. |

State bundles:L loading->populated+long text/code/paths;E empty/zero;R error+retry;U unavailable/unknown/unsupported/encrypted where supported;P read-only/denied/edit/admin actor variants;K keyboard/focus/native-dialog/edit/busy/protected-draft/return/anchors. Every existing subsurface's supported bundle states needs its own DOM/functional assertions;retain one highest-risk representativePNG per family/bundle and record membership/substeps/outcomes. Inapplicable states need a source/control reason. Only common geometry/style evidence can reuse B/E;authority,draft and mutation assertions stay domain-specific. This is pairwise/bundle coverage,not a Cartesian claim.

Total60pairs plus72candidate-only screenshots=192PNG and24zero-image boundary cases,within256MiB. One contained `output/ui-alignment-27e2d615/` area is owned by the final qualifier through project closure,then newly created evidence is disposed. Preserve accepted originals and preexisting evidence/output siblings. Replace disposed superseded current-task captures or justify a revised bound for extra unresolved diagnostics;do not overwrite historical evidence.

## Validation and delivery

Shared shell precedes Projects,then Sessions;other families follow shell. Final qualification follows all six families and independent acceptance follows qualification. One source owner controls shared files per window;each source patch needs independent exact-patch review before explicit-path commit.

Run supported `npm test`,`npm run build` and affected package/browser suites at matching inputs. Record compact commands,outcomes,input hashes and limitations;reuse valid results only with matching relevant inputs. Native integration uses candidate-local executables with SHA-256 provenance;never install/copy global binaries or change services. Full performance qualification remains separate. Planning acceptance does not establish rendered fidelity or functional acceptance.