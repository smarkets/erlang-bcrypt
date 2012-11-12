#ifndef ERLANG_BCRYPT_BCRYPT_NIF_H
#define ERLANG_BCRYPT_BCRYPT_NIF_H

#include "async_queue.h"

typedef unsigned char byte;

int bcrypt(char *, const char *, const char *);
void encode_salt(char *, u_int8_t *, u_int16_t, u_int8_t);

typedef struct {
    ErlNifResourceType *bcrypt_rt;
} bcrypt_privdata_t;

typedef struct {
    async_queue_t    *queue;
    ErlNifThreadOpts *topts;
    ErlNifTid         tid;
} ctx_t;

typedef enum {
    UNKNOWN,
    SHUTDOWN,
    HASH
} task_type_t;

typedef struct {
    task_type_t  type;
    ErlNifEnv   *env;
    ErlNifPid    pid;
    ERL_NIF_TERM ref;
    union {
        struct {
            ErlNifBinary salt;
            ErlNifBinary password;
        } hash;
    } data;
} task_t;

#endif  // ERLANG_BCRYPT_BCRYPT_NIF_H
