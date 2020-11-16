#include "test_core.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint64_t u64;

#define FOR(i, start, end) for (size_t i = (start); (i) < (end); (i)++)

////////////////////
/// Dependencies ///
////////////////////
#include "monocypher.h"

static void kdf(u8 next[48], const u8 prev[32], const u8 *in, size_t size)
{
    crypto_blake2b_general(next, 48, prev, 32, in, size);
}

static void public_key(u8 pk[32], const u8 sk[32])
{
    crypto_x25519_public_key(pk, sk);
}

// in == NULL is the same as in == {0, 0, 0, ...}
static void encrypt(u8 *out, const u8 *in, size_t size, u8 key[32])
{
    u8 nonce[8] = {0};
    crypto_chacha20(out, in, size, key, nonce);
}

static void key_exchange(u8 shared_secret[32], const u8 sk[32], const u8 pk[32])
{
    crypto_x25519(shared_secret, sk, pk);
}
////////////////////////
/// end dependencies ///
////////////////////////

// Pseudo-random 64 bit number, based on xorshift*
static u64 rand64()
{
    static u64 x = 12345; // Must be seeded with a nonzero value.
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    return x * 0x2545F4914F6CDD1D; // magic constant
}

void p_random(u8 *stream, size_t size)
{
    FOR (i, 0, size) {
        stream[i] = (u8)rand64();
    }
}

typedef struct {
    u8     prelude_buf     [32];
    u8     payload_bufs [4][32];
    u8    *prelude;           // &prelude_buf  or NULL
    u8    *payloads     [4];  // &payload_bufs or NULL
    size_t prelude_size;
    size_t payload_sizes[4];
} inputs;

typedef struct {
    u8 session_key[32];
    u8 remote_key [32];
} outputs;

typedef struct {
    u8     buf[256];     // maximum message size
    size_t size;         // how many bytes in the buffer
    size_t corrupt_in;   // if 0, corrupt next message
    size_t corrupt_at;   // byte to corrupt
    u8     corrupt_with; // Which bits we flip (XOR)
} network;

// Context status flags
static const u16 IS_OK       =  1; // Always 1 (becomes zero when wiped)
static const u16 HAS_REMOTE  =  4; // True if we have the remote DH key
static const u16 GETS_REMOTE =  8; // True if the remote key is wanted

// message tokens
typedef enum { NOOP=0, E=1, S=2, EE=3, ES=4, SE=5, SS=6 } action;

static int has_prelude(unsigned flags)             { return  flags       & 1; }
static int has_payload(unsigned flags, unsigned i) { return (flags >> i) & 2; }

static void make_inputs(inputs *in, unsigned flags)
{
    if (has_prelude(flags)) {
        size_t size = rand64() % 32;
        p_random(in->prelude_buf, size);
        in->prelude      = in->prelude_buf;
        in->prelude_size = size;
    } else {
        in->prelude      = 0;
        in->prelude_size = 0;
    }
    FOR (i, 0, 4) {
        if (has_payload(flags, (unsigned)i)) {
            size_t size = rand64() % 32;
            p_random(in->payload_bufs[i], size);
            in->payloads     [i] = in->payload_bufs[i];
            in->payload_sizes[i] = size;
        } else {
            in->payloads     [i] = 0;
            in->payload_sizes[i] = 0;
        }
    }
}

static network corrupt_network(size_t msg, size_t at, u8 corruptor)
{
    network n;
    n.size = 0; // nothing written yet
    n.corrupt_in   = msg;
    n.corrupt_at   = at;
    n.corrupt_with = corruptor;
    return n;
}

static network clean_network()
{
    return corrupt_network((size_t)-1, 0, 0);
}

static void network_read(network *n, u8 *buf, size_t size)
{
    if (n->size != 0) {
        assert(n->size == size); // only read exactly what has been written
        memcpy(buf, n->buf, size);
        n->size = 0;
    } else {
        // if the network is empty, fill the buffer with garbage
        memset(buf, 0, size);
    }
}

static void network_write(network *n, const u8 *buf, size_t size)
{
    assert(n->size == 0);
    assert(size <= sizeof(n->buf)); // just so we don't overflow the network
    memcpy(n->buf, buf, size);
    n->size = size;
    if (n->corrupt_in == 0) {
        assert(n->corrupt_at < size); // never corrupt out of bounds
        n->buf[n->corrupt_at] ^= n->corrupt_with;
    }
    n->corrupt_in--;
}

static size_t step(crypto_kex_ctx *ctx, outputs *out,
                   network  *net,
                   u8       *p_out, size_t p_osize,
                   const u8 *p_in , size_t p_isize,
                   size_t    padding)
{
    crypto_kex_action action;
    size_t m_size;
    action = crypto_kex_next_action(ctx, &m_size);
    size_t message_size = 0;
    do {
        switch (action) {
        case CRYPTO_KEX_READ: {
            assert(ctx->flags & IS_OK);
            u8 m_in[256];
            size_t read_size = m_size + p_osize + padding;
            network_read(net, m_in, read_size);
            int ko = crypto_kex_read_p(ctx, p_out, p_osize, m_in, read_size);
            if (ko) {
                assert(!(ctx->flags & IS_OK));
                return (size_t)-1; // protocol failure
            } else {
                assert(ctx->flags & IS_OK);
            }
        } break;
        case CRYPTO_KEX_WRITE: {
            u8 m_out[256];
            size_t write_size = m_size + p_isize + padding;
            crypto_kex_write_p(ctx, m_out, write_size, p_in, p_isize);
            network_write(net, m_out, write_size);
            assert(ctx->flags & IS_OK);
            message_size = m_size + p_isize;
        } break;
        case CRYPTO_KEX_REMOTE_KEY:
            crypto_kex_remote_key(ctx, out->remote_key);
            assert(ctx->flags & IS_OK);
            break;
        case CRYPTO_KEX_FINAL:
            crypto_kex_final(ctx, out->session_key);
            assert(ctx->flags == 0);
            break;
        case CRYPTO_KEX_NONE:
            break;
        default:
            assert(0);
        }
        action = crypto_kex_next_action(ctx, &m_size);
    } while (action != CRYPTO_KEX_NONE &&
             action != CRYPTO_KEX_READ);
    return message_size;
}

static void load_pattern(action pattern[4][5], const crypto_kex_ctx *ctx)
{
    FOR (i, 0, 4) {
        u16 message = ctx->messages[i];
        FOR (j, 0, 5) {
            pattern[i][j] = message & 7;
            message >>= 3;
        }
    }
}

static void mix_hash(u8 hash[64], const u8 *in, size_t size)
{
    kdf(hash, hash, in, size);
}

static void e_mix_hash(u8 hash[64], const u8 *in, size_t size)
{
    u8 *key = hash + 32;
    u8  tmp[128];
    encrypt(hash, 0, 64, hash);  // split an encryption key
    encrypt(tmp, in, size, key);
    mix_hash(hash, tmp , size);  // absorb encrypted message
}

static void exchange(u8 hash[64], u8 s1[32], u8 p1[32], u8 s2[32], u8 p2[32])
{
    u8 t1[32], t2[32];
    key_exchange(t1, s1, p2);
    key_exchange(t2, s2, p1);
    assert(!memcmp(t1, t2, 32));
    mix_hash(hash, t1, 32);
}

static size_t nb_messages(const crypto_kex_ctx *ctx)
{
    FOR (i, 0, 4) {
        if (ctx->messages[i] == 0) {
            return i;
        }
    }
    return 4;
}

static void session_vectors(outputs              *out,
                            const inputs         *in,
                            const crypto_kex_ctx *client,
                            const crypto_kex_ctx *server,
                            const u8              pattern_id[64])
{
    // Pattern
    action client_pattern[4][5];  load_pattern(client_pattern, client);
    action server_pattern[4][5];  load_pattern(server_pattern, server);
    FOR (i, 0, 4) {
        FOR (j, 0, 5) {
            action token = server_pattern[i][j];
            if (token == SE) { server_pattern[i][j] = ES; }
            if (token == ES) { server_pattern[i][j] = SE; }
            assert(client_pattern[i][j] == server_pattern[i][j]);
        }
    }
    size_t nb_msg = nb_messages(client);
    assert(nb_msg == nb_messages(server));

    // keys
    int has_cs = server->flags & (HAS_REMOTE | GETS_REMOTE);
    int has_ss = client->flags & (HAS_REMOTE | GETS_REMOTE);
    int has_ce = 1; // client always sends a message
    int has_se = nb_msg >= 2;

    u8 ces[32], cep[32], ses[32], sep[32];
    u8 css[32], csp[32], sss[32], ssp[32];
    if (has_ce){memcpy(ces, client->e, 32); public_key(cep, ces);}
    if (has_se){memcpy(ses, server->e, 32); public_key(sep, ses);}
    if (has_cs){memcpy(css, client->s, 32); public_key(csp, css);}
    if (has_ss){memcpy(sss, server->s, 32); public_key(ssp, sss);}
    if (has_cs) { assert(!memcmp(client->sp, csp, 32)); }
    if (has_ss) { assert(!memcmp(server->sp, ssp, 32)); }

    // Initial hash
    u8 hash[64];
    memcpy(hash, pattern_id, 32);
    if (server->flags & HAS_REMOTE) {
        assert(!memcmp(client->sp, server->sr, 32));
        kdf(hash, hash, client->sp, 32);
    }
    if (client->flags & HAS_REMOTE) {
        assert(!memcmp(server->sp, client->sr, 32));
        kdf(hash, hash, server->sp, 32);
    }
    assert(!memcmp(client->hash, hash, 32)); // check client initial hash
    assert(!memcmp(server->hash, hash, 32)); // check server initial hash

    // Prelude
    if (in->prelude) {
        mix_hash(hash, in->prelude, in->prelude_size);
    }

    int has_key = 0; // can encrypt
    FOR (i, 0, 4) {
        if (client_pattern[i][0] == NOOP) {
            break;
        }
        FOR (j, 0, 5) {
            switch (client_pattern[i][j]) {
            case EE  : exchange(hash, ces, cep, ses, sep); has_key = 1; break;
            case ES  : exchange(hash, ces, cep, sss, ssp); has_key = 1; break;
            case SE  : exchange(hash, css, csp, ses, sep); has_key = 1; break;
            case SS  : exchange(hash, css, csp, sss, ssp); has_key = 1; break;
            case E   :
                mix_hash(hash, i%2 == 0 ? client->ep : server->ep, 32);
                break;
            case S   :
                if (has_key) { e_mix_hash(hash, i%2 == 0 ? csp : ssp, 32); }
                else         {   mix_hash(hash, i%2 == 0 ? csp : ssp, 32); }
                break;
            case NOOP: break;
            default  : assert(0);
            }
        }
        const u8 *payload = in->payloads[i];
        size_t    size    = in->payload_sizes[i];
        if (has_key) { e_mix_hash(hash, payload, size); }
        else         {   mix_hash(hash, payload, size); }
    }

    memcpy(out->session_key, hash     , 32);
}

static void session(outputs              *co, // client out
                    outputs              *so, // server out
                    u8                    payloads[4][32],
                    size_t                msg_sizes[5],
                    network              *net,
                    const inputs         *in,
                    const crypto_kex_ctx *client,
                    const crypto_kex_ctx *server,
                    size_t                padding)
{
    crypto_kex_ctx c = *client;
    crypto_kex_ctx s = *server;
    // Prelude
    if (in->prelude) {
        crypto_kex_add_prelude(&c, in->prelude, in->prelude_size);
        crypto_kex_add_prelude(&s, in->prelude, in->prelude_size);
        assert(!memcmp(c.hash, s.hash, 48));
    }

    // Protocol
    assert(crypto_kex_next_action(&c, 0) != CRYPTO_KEX_NONE);
    assert(crypto_kex_next_action(&s, 0) != CRYPTO_KEX_NONE);

    u8    *po[4] = {0};
    u8    *pi[4] = {0};
    size_t ps[4] = {0};
    FOR (i, 0, 4) {
        if (in->payloads[i]) {
            pi[i] = in->payloads     [i];
            ps[i] = in->payload_sizes[i];
            po[i] = payloads         [i];
        }
    }

    msg_sizes[0] = step(&c, co, net, 0    , 0    , pi[0], ps[0], padding);
    msg_sizes[1] = step(&s, so, net, po[0], ps[0], pi[1], ps[1], padding);
    msg_sizes[2] = step(&c, co, net, po[1], ps[1], pi[2], ps[2], padding);
    msg_sizes[3] = step(&s, so, net, po[2], ps[2], pi[3], ps[3], padding);
    msg_sizes[4] = step(&c, co, net, po[3], ps[3], 0    , 0    , padding);

    assert(crypto_kex_next_action(&c, 0) == CRYPTO_KEX_NONE);
    assert(crypto_kex_next_action(&s, 0) == CRYPTO_KEX_NONE);
}

static void session_success(size_t msg_sizes[5],     // actual message sizes
                            u8     payloads [4][32], // transmitted payloads
                            const crypto_kex_ctx *client,
                            const crypto_kex_ctx *server,
                            const inputs         *in,
                            const outputs        *vectors,
                            size_t padding)
{
    // Sucessful session
    outputs out_client;
    outputs out_server;
    network net = clean_network();
    session(&out_client, &out_server, payloads, msg_sizes, &net,
            in, client, server, padding);

    // No error after sucessful session
    FOR (i, 0, 5) {
        assert(msg_sizes[i] != ((size_t)-1));
    }
    assert(msg_sizes[4] == 0); // there is no message 4
    // Sucessful session faithfully transmits payloads
    size_t nb_msg = nb_messages(client);
    FOR (i, 0, nb_msg) {
        if (in->payloads[i]) {
            assert(!memcmp(payloads         [i],
                           in->payloads     [i],
                           in->payload_sizes[i]));
        }
    }
    // Sucessful session faithfully transmits the remote key
    if (client->flags & GETS_REMOTE) {
        assert(!memcmp(server->sp, out_client.remote_key, 32));
    }
    if (server->flags & GETS_REMOTE) {
        assert(!memcmp(client->sp, out_server.remote_key, 32));
    }
    // Sucessful session agrees with the test vectors
    assert(!memcmp(out_client.session_key, vectors->session_key, 32));
    assert(!memcmp(out_server.session_key, vectors->session_key, 32));
}


void test_pattern(const crypto_kex_ctx *client,
                  const crypto_kex_ctx *server,
                  const u8              pattern_id[64])
{
    size_t nb_msg   = nb_messages(client);
    size_t nb_flags = (size_t)2 << nb_msg;
    assert(nb_flags >=  4);
    assert(nb_flags <= 32);
    FOR (flags, 0, nb_flags) {
        // test vectors
        inputs  in;  make_inputs(&in, (unsigned)flags);
        outputs vectors;
        size_t  msg_sizes[5];     // size of each message in a session
        u8      payloads [4][32]; // transmitted payloads
        session_vectors(&vectors, &in, client, server, pattern_id);
        session_success(msg_sizes, payloads, client, server, &in, &vectors,128);
        session_success(msg_sizes, payloads, client, server, &in, &vectors,  0);

        // Failing sessions (network corruption)
        FOR (i, 0, 4) {
            if (msg_sizes[i] == 0) {
                break;
            }
            // Corrupt each byte of the message.
            //
            // Note 1: we corrrupt the most significant bit, so we
            // sometimes flip the most significant bit of public keys.
            // This operation has no effect on the key exchanges, but
            // should still result in failure. (The protocol is supposed
            // to check the integrity of the whole transcript).
            //
            // Note 2: corrupting a payload, even if it is unencrypted,
            // should result in failure: at the end of the handshake,
            // the whole transcript is authenticated.
            u8 corrupt_with = 128;
            FOR (corrupt_at, 0, msg_sizes[i]) {
                outputs out_client;
                outputs out_server;
                size_t  corrupt_sizes[5];
                network net = corrupt_network(i, corrupt_at, corrupt_with);
                session(&out_client, &out_server, payloads, corrupt_sizes, &net,
                        &in, client, server, 0);
                // Check that the session failed visibly.
                int ko = 0;
                FOR (j, 0, 5) {
                    ko = ko || corrupt_sizes[j] == (size_t)-1;
                }
                assert(ko);
            }
        }
    }
    printf("OK: %s\n", pattern_id);
}
