/* zlib's ABI stays on this side of the FFI. No zlib counters are authoritative. */
#include <stdint.h>
#include <stdlib.h>
#include <zlib.h>

typedef struct { z_stream stream; gz_header header; int encode; } hmem_codec;

const char *hmem_archive_zlib_version(void) { return zlibVersion(); }

void *hmem_archive_codec_new(int encode) {
    hmem_codec *c = calloc(1, sizeof(*c));
    if (!c) return NULL;
    c->encode = encode;
    int result = encode
        ? deflateInit2(&c->stream, 6, Z_DEFLATED, 31, 8, Z_DEFAULT_STRATEGY)
        : inflateInit2(&c->stream, 31);
    if (result != Z_OK) { free(c); return NULL; }
    if (encode) {
        c->header.os = 255;
        if (deflateSetHeader(&c->stream, &c->header) != Z_OK) {
            deflateEnd(&c->stream); free(c); return NULL;
        }
    }
    return c;
}

void hmem_archive_codec_free(void *opaque) {
    hmem_codec *c = opaque;
    if (!c) return;
    if (c->encode) deflateEnd(&c->stream); else inflateEnd(&c->stream);
    free(c);
}

/* mode: 0 ordinary input, 1 dictionary-preserving block, 2 finalization.
 * Every pointer is borrowed for this call only. Counts fit even 16-bit uInt. */
int hmem_archive_codec_step(void *opaque, const unsigned char *input,
    uint32_t length, unsigned char *output, uint32_t capacity, int mode,
    uint32_t *consumed, uint32_t *produced, uint32_t *boundary) {
    hmem_codec *c = opaque;
    if (!c || length > 65536 || capacity != 65536 || mode < 0 || mode > 2 ||
        (uint64_t)(uInt)length != length || (uint64_t)(uInt)capacity != capacity)
        return Z_STREAM_ERROR;
    c->stream.next_in = (Bytef *)input; c->stream.avail_in = (uInt)length;
    c->stream.next_out = output; c->stream.avail_out = (uInt)capacity;
    int result = c->encode
        ? deflate(&c->stream, mode == 2 ? Z_FINISH : mode == 1 ? Z_BLOCK : Z_NO_FLUSH)
        : inflate(&c->stream, Z_BLOCK);
    *consumed = length - c->stream.avail_in;
    *produced = capacity - c->stream.avail_out;
    *boundary = (uint32_t)c->stream.data_type;
    c->stream.next_in = NULL; c->stream.next_out = NULL;
    c->stream.avail_in = 0; c->stream.avail_out = 0;
    return result;
}

int hmem_archive_dictionary(void *opaque, unsigned char *output, uint32_t *length) {
    hmem_codec *c = opaque; uInt count = 0;
    if (!c || c->encode) return Z_STREAM_ERROR;
    int result = inflateGetDictionary(&c->stream, output, &count);
    if (count > 32768) return Z_STREAM_ERROR;
    *length = count;
    return result;
}
