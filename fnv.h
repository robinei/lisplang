#ifndef FNV_H
#define FNV_H

#include <stdint.h>

#define FNV_PRIME 0x01000193
#define FNV_SEED 0x811C9DC5

static uint32_t fnv1a(unsigned char *ptr, uint32_t len, uint32_t hash) {
    while (len--) {
        hash = (*ptr++ ^ hash) * FNV_PRIME;
    }
    return hash;
}

#endif
