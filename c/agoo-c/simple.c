// Copyright 2018 by Peter Ohler, All Rights Reserved

#include <stdio.h>
#include <stdlib.h>

#include <agoo.h>
#include <agoo/log.h>
#include <agoo/page.h>
#include <agoo/server.h>

static int	user_off = 6;

static void
user_handler(Req req) {
    agoo_respond(req, 200, req->path.start + user_off, req->path.len - user_off, NULL);
}

static void
empty_handler(Req req) {
    agoo_respond(req, 200, NULL, 0, NULL);
}

int
main(int argc, char **argv) {
    struct _Err	err = ERR_INIT;
    
    agoo_init("simple");

    // Set the number of eval threads.
    the_server.thread_cnt = 1;

    pages_set_root(".");

    if (ERR_OK != agoo_bind_port(&err, 3000)) {
	printf("Failed to bind to port. %s\n", err.msg);
	return err.code;
    }
    // set up hooks or routes
    if (ERR_OK != agoo_add_func_hook(&err, GET, "/", empty_handler, true) ||
	ERR_OK != agoo_add_func_hook(&err, GET, "/user/*", user_handler, true) ||
	ERR_OK != agoo_add_func_hook(&err, POST, "/user", empty_handler, true)) {
	return err.code;
    }
    // start the server and wait for it to be shutdown
    if (ERR_OK != agoo_start(&err, "0.1")) {
	printf("%s\n", err.msg);
	return err.code;
    }
    return 0;
}
