/*
 * Copyright (c) 2011-2012 Hunter Morris <hunter.morris@smarkets.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "erl_nif.h"
#include "erl_blf.h"
#include "bcrypt_nif.h"

void free_task(task_t* task)
{
    if (task->env != NULL)
        enif_free_env(task->env);
    enif_free(task);
}

task_t* alloc_task(task_type_t type)
{
    task_t* task = (task_t*)enif_alloc(sizeof(task_t));
    if (task == NULL)
        return NULL;
    (void)memset(task, 0, sizeof(task_t));
    task->type = type;
    return task;
}

task_t* alloc_init_task(task_type_t type, ERL_NIF_TERM ref, ErlNifPid pid, int num_orig_terms, const ERL_NIF_TERM orig_terms[])
{
    task_t* task = alloc_task(type);
    task->pid = pid;
    task->env = enif_alloc_env();
    if (task->env == NULL) {
        free_task(task);
        return NULL;
    }

    if (type == HASH) {
        assert(num_orig_terms == 2);
        if (!enif_inspect_iolist_as_binary(
                task->env, enif_make_copy(task->env, orig_terms[0]),
                &task->data.hash.salt)) {
            free_task(task);
            return NULL;
        }
        if (!enif_inspect_iolist_as_binary(
                task->env, enif_make_copy(task->env, orig_terms[1]),
                &task->data.hash.password)) {
            free_task(task);
            return NULL;
        }
    }

    task->ref = enif_make_copy(task->env, ref);
    return task;
}

static ERL_NIF_TERM hashpw(task_t* task)
{
    char password[1024] = { 0 };
    char salt[1024] = { 0 };
    char encrypted[1024] = { 0 };

    size_t password_sz = 1024;
    if (password_sz > task->data.hash.password.size)
        password_sz = task->data.hash.password.size;
    (void)memcpy(&password, task->data.hash.password.data, password_sz);

    size_t salt_sz = 1024;
    if (salt_sz > task->data.hash.salt.size)
        salt_sz = task->data.hash.salt.size;
    (void)memcpy(&salt, task->data.hash.salt.data, salt_sz);

    if (bcrypt(encrypted, password, salt)) {
        return enif_make_tuple3(
            task->env,
            enif_make_atom(task->env, "error"),
            task->ref,
            enif_make_string(task->env, "bcrypt failed", ERL_NIF_LATIN1));
    }

    return enif_make_tuple3(
        task->env,
        enif_make_atom(task->env, "ok"),
        task->ref,
        enif_make_string(task->env, encrypted, ERL_NIF_LATIN1));
}

void* async_worker(void* arg)
{
    ctx_t* ctx;
    task_t* task;

    ERL_NIF_TERM result;

    ctx = (ctx_t*)arg;

    while (1) {
        task = (task_t*)async_queue_pop(ctx->queue);

        if (task->type == SHUTDOWN) {
            free_task(task);
            break;
        } else if (task->type == HASH) {
            result = hashpw(task);
        } else {
            errx(1, "Unexpected task type: %i", task->type);
        }

        enif_send(NULL, &task->pid, task->env, result);
        free_task(task);
    }

    return NULL;
}

static ERL_NIF_TERM bcrypt_encode_salt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary csalt, bin;
    unsigned long log_rounds;
    ERL_NIF_TERM ret;

    if (!enif_inspect_binary(env, argv[0], &csalt) || 16 != csalt.size) {
        return enif_make_badarg(env);
    }

    if (!enif_get_ulong(env, argv[1], &log_rounds)) {
        enif_release_binary(&csalt);
        return enif_make_badarg(env);
    }

    if (!enif_alloc_binary(64, &bin)) {
        enif_release_binary(&csalt);
        return enif_make_badarg(env);
    }

    encode_salt((char *)bin.data, (u_int8_t*)csalt.data, csalt.size, log_rounds);
    enif_release_binary(&csalt);

    ret = enif_make_string(env, (char *)bin.data, ERL_NIF_LATIN1);
    enif_release_binary(&bin);
    return ret;
}

static ERL_NIF_TERM bcrypt_hashpw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ctx_t *ctx;
    task_t *task;
    ErlNifPid pid;

    if (argc != 5)
        return enif_make_badarg(env);

    bcrypt_privdata_t *priv = (bcrypt_privdata_t*)enif_priv_data(env);

    if (!enif_get_resource(env, argv[0], priv->bcrypt_rt, (void**)(&ctx)))
        return enif_make_badarg(env);

    if (!enif_is_ref(env, argv[1]))
        return enif_make_badarg(env);

    if (!enif_get_local_pid(env, argv[2], &pid))
        return enif_make_badarg(env);

    ERL_NIF_TERM orig_terms[] = { argv[4], argv[3] };
    task = alloc_init_task(HASH, argv[1], pid, 2, orig_terms);

    if (!task)
        return enif_make_badarg(env);

    async_queue_push(ctx->queue, task);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM bcrypt_create_ctx(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM ret;
    bcrypt_privdata_t *priv = (bcrypt_privdata_t*)enif_priv_data(env);
    ctx_t* ctx = (ctx_t*)enif_alloc_resource(priv->bcrypt_rt, sizeof(ctx_t));
    if (ctx == NULL)
        return enif_make_badarg(env);
    ctx->queue = async_queue_create("bcrypt_queue_mutex", "bcrypt_queue_condvar");
    ctx->topts = enif_thread_opts_create("bcrypt_thread_opts");
    if (enif_thread_create("bcrypt_worker", &ctx->tid, async_worker, ctx, ctx->topts) != 0) {
        enif_release_resource(ctx);
        return enif_make_badarg(env);
    }
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}

static ErlNifFunc bcrypt_nif_funcs[] =
{
    {"encode_salt", 2, bcrypt_encode_salt},
    {"hashpw", 5, bcrypt_hashpw},
    {"create_ctx", 0, bcrypt_create_ctx},
};

static void bcrypt_rt_dtor(ErlNifEnv* env, void* obj)
{
    ctx_t  *ctx = (ctx_t*)obj;
    task_t *task = alloc_task(SHUTDOWN);
    void   *result = NULL;

    async_queue_push(ctx->queue, task);
    enif_thread_join(ctx->tid, &result);
    async_queue_destroy(ctx->queue);
    enif_thread_opts_destroy(ctx->topts);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    const char *mod = "bcrypt_nif";
    const char *name = "nif_resource";

    ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);

    bcrypt_privdata_t *priv = (bcrypt_privdata_t*)enif_alloc(sizeof(bcrypt_privdata_t));
    priv->bcrypt_rt = enif_open_resource_type(env, mod, name, bcrypt_rt_dtor, flags, NULL);
    if (priv->bcrypt_rt == NULL)
        return -1;
    *priv_data = priv;
    return 0;
}

ERL_NIF_INIT(bcrypt_nif, bcrypt_nif_funcs, &on_load, NULL, NULL, NULL)
