#include "monocypher.h"

static void keyed_hash(u8 hash[64], const u8 key[64], const u8 *in, size_t size)
{
    crypto_blake2b_general(hash, 64, key, 64, in, size);
}

static void ephemeral_key_pair(u8 pk[32], u8 sk[32], u8 seed[32])
{
    copy(sk, seed, 32);
    crypto_x25519_public_key(pk, sk);
}

static void static_public_key(u8 pk[32], const u8 sk[32])
{
    crypto_x25519_public_key(pk, sk);
}

#define decode_ephemeral_key(key) // No-op

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
