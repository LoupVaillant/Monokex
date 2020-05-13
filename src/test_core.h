#ifndef TEST_CORE_H
#define TEST_CORE_H

#include "monokex.h"

void p_random(uint8_t *stream, size_t size);

void test_pattern(const crypto_kex_ctx *client,
                  const crypto_kex_ctx *server,
                  const uint8_t         pattern_id[64]);

#endif // TEST_CORE_H
