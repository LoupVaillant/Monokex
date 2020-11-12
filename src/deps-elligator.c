#include "monocypher.h"

static void kdf(u8 next[64], const u8 prev[32], const u8 *in, size_t size)
{
    crypto_blake2b_general(next, 48, prev, 32, in, size);
}

static void ephemeral_key_pair(u8 pk[32], u8 sk[32], u8 seed[32])
{
    crypto_hidden_key_pair(pk, sk, seed);
}

static void static_public_key(u8 pk[32], const u8 sk[32])
{
    crypto_x25519_public_key(pk, sk);
}

// If the key is hidden, unhide them
static void decode_ephemeral_key(u8 key[32])
{
    crypto_hidden_to_curve(key, key);
}

// in == NULL is the same as in == {0, 0, 0, ...}
static void encrypt(u8 *out, const u8 *in, size_t size, u8 key[32])
{
    crypto_chacha20(out, in, size, key, zero);
}

static void key_exchange(u8 shared_secret[32], const u8 sk[32], const u8 pk[32])
{
    crypto_x25519(shared_secret, sk, pk);
}

// Runs in constant time
static int verify16(const u8 a[16], const u8 b[16])
{
    return crypto_verify16(a, b);
}

// securely erases memory
static void wipe(void *buffer, size_t size)
{
    crypto_wipe(buffer, size);
}
