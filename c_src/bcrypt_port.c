/*
 * Copyright (c) 2011 Hunter Morris <hunter.morris@smarkets.com>
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <erl_interface.h>
#include <ei.h>
#include <unistd.h>

#include "erl_blf.h"

#define dec_int16(s) ((((unsigned char*)(s))[0] << 8) | \
                      (((unsigned char*)(s))[1]))

#define enc_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff;  \
        ((unsigned char*)(s))[1] = (i) & 0xff;}

#define BUFSIZE (1 << 16)
#define CMD_SALT 0
#define CMD_HASHPW 1

typedef unsigned char byte;

char *bcrypt(const char *, const char *);
void encode_salt(char *, u_int8_t *, u_int16_t, u_int8_t);

/* These methods came from the Erlang port command tutorial:
 * http://www.erlang.org/doc/tutorial/c_port.html#4.2
 */
static int
read_buf(int fd, byte *buf, int len)
{
    int i, got = 0;
    do {
        if ((i = read(fd, buf+got, len-got)) <= 0) {
            if (i == 0) return got;
            if (errno != EINTR)
                return got;
            i = 0;
        }
        got += i;
    } while (got < len);
    return (len);
}

static int
read_cmd(byte *buf)
{
    int len;
    if (read_buf(0, buf, 2) != 2)
        return 0;
    len = dec_int16(buf);
    if (read_buf(0, buf, len) != len)
        return 0;
    return 1;
}

static int
write_buf(int fd, byte *buf, int len)
{
    int i, done = 0;
    do {
        if ((i = write(fd, buf+done, len-done)) < 0) {
            if (errno != EINTR)
                return (i);
            i = 0;
        }
        done += i;
    } while (done < len);
    return (len);
}

static int
write_cmd(byte *buf, int len)
{
    byte hd[2];
    enc_int16(len, hd);
    if (write_buf(1, hd, 2) != 2)
        return 0;
    if (write_buf(1, buf, len) != len)
        return 0;
    return 1;
}

static int
process_reply(ETERM *pid, int cmd, ETERM *res)
{
    ETERM *result;
    int len, retval;
    byte *buf;
    result = erl_format("{~i, ~w, ~w}", cmd, pid, res);
    len = erl_term_len(result);
    buf = erl_malloc(len);
    erl_encode(result, buf);
    retval = write_cmd(buf, len);
    erl_free_term(result);
    erl_free(buf);
    return retval;
}

static int
process_reply_error(ETERM *pid, int cmd, const char *res)
{
    ETERM *result;
    int len, retval;
    byte *buf;
    result = erl_format("{~i, ~w, ~s}", cmd, pid, res);
    len = erl_term_len(result);
    buf = erl_malloc(len);
    erl_encode(result, buf);
    retval = write_cmd(buf, len);
    erl_free_term(result);
    erl_free(buf);
    return retval;
}

static int
process_encode_salt(ETERM *pid, ETERM *data)
{
    int retval = 0;
    ETERM *pattern, *cslt, *lr, *ret_bin;
    byte *csalt = NULL;
    long log_rounds = -1;
    int csaltlen = -1;
    char ret[64];
    pattern = erl_format("{Csalt, LogRounds}");
    if (erl_match(pattern, data)) {
        cslt = erl_var_content(pattern, "Csalt");
        csaltlen = ERL_BIN_SIZE(cslt);
        csalt = ERL_BIN_PTR(cslt);
        lr = erl_var_content(pattern, "LogRounds");
        log_rounds = ERL_INT_UVALUE(lr);
        if (16 != csaltlen) {
            retval = process_reply_error(pid, CMD_SALT, "Invalid salt length");
        } else if (log_rounds < 4 || log_rounds > 31) {
            retval = process_reply_error(pid, CMD_SALT, "Invalid number of rounds");
        } else {
            encode_salt(ret, (u_int8_t*)csalt, csaltlen, log_rounds);
            ret_bin = erl_mk_binary(ret, strlen(ret));
            retval = process_reply(pid, CMD_SALT, ret_bin);
            erl_free_term(ret_bin);
        }
        erl_free_term(cslt);
        erl_free_term(lr);
    };
    erl_free_term(pattern);
    return retval;
}

static int
process_hashpw(ETERM *pid, ETERM *data)
{
    int retval = 0;
    ETERM *pattern, *pwd, *slt, *pwd_bin, *slt_bin, *ret_bin;
    char password[1024];
    char salt[1024];
    char *ret = NULL;

    (void)memset(&password, '\0', sizeof(password));
    (void)memset(&salt, '\0', sizeof(salt));

    pattern = erl_format("{Pass, Salt}");
    if (erl_match(pattern, data)) {
        pwd = erl_var_content(pattern, "Pass");
        pwd_bin = erl_iolist_to_binary(pwd);
        slt = erl_var_content(pattern, "Salt");
        slt_bin = erl_iolist_to_binary(slt);
        if (ERL_BIN_SIZE(pwd_bin) > sizeof(password)) {
            retval = process_reply_error(pid, CMD_HASHPW, "Password too long");
        } else if (ERL_BIN_SIZE(slt_bin) > sizeof(salt)) {
            retval = process_reply_error(pid, CMD_HASHPW, "Salt too long");
        } else {
            memcpy(password, ERL_BIN_PTR(pwd_bin), ERL_BIN_SIZE(pwd_bin));
            memcpy(salt, ERL_BIN_PTR(slt_bin), ERL_BIN_SIZE(slt_bin));
            if (NULL == (ret = bcrypt(password, salt)) ||
                0 == strcmp(ret, ":")) {
                retval = process_reply_error(pid, CMD_HASHPW, "Invalid salt");
            } else {
                ret_bin = erl_mk_binary(ret, strlen(ret));
                retval = process_reply(pid, CMD_HASHPW, ret_bin);
                erl_free_term(ret_bin);
            }
        }
        erl_free_term(pwd);
        erl_free_term(slt);
        erl_free_term(pwd_bin);
        erl_free_term(slt_bin);
    };
    erl_free_term(pattern);
    return retval;
}

static int
process_command(unsigned char *buf)
{
    int retval = 0;
    ETERM *pattern, *tuple, *cmd, *port, *data;
    pattern = erl_format("{Cmd, Port, Data}");
    tuple = erl_decode(buf);
    if (erl_match(pattern, tuple)) {
        cmd = erl_var_content(pattern, "Cmd");
        port = erl_var_content(pattern, "Port");
        data = erl_var_content(pattern, "Data");
        switch (ERL_INT_VALUE(cmd)) {
        case CMD_SALT:
            retval = process_encode_salt(port, data);
            break;
        case CMD_HASHPW:
            retval = process_hashpw(port, data);
            break;
        };
        erl_free_term(cmd);
        erl_free_term(port);
        erl_free_term(data);
    }
    erl_free_term(pattern);
    erl_free_term(tuple);
    return retval;
}

static void
loop(void)
{
    byte buf[BUFSIZE];
    int retval = 0;
    do {
        if (read_cmd(buf) > 0)
            retval = process_command(buf);
        else
            retval = 0;
    } while (retval);
}

int
main(int argc, char *argv[])
{
    erl_init(NULL, 0);
    loop();
    return 0;
}
