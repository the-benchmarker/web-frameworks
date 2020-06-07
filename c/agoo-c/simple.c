/**
  Copyright 2018 by Peter Ohler, All Rights Reserved
  */

#include <stdio.h>
#include <stdlib.h>

#include <agoo.h>
#include <agoo/log.h>
#include <agoo/page.h>
#include <agoo/res.h>
#include <agoo/server.h>

static agooText emptyResp = NULL;

static void empty_handler(agooReq req) {
  if (NULL == emptyResp) {
    emptyResp = agoo_respond(200, NULL, 0, NULL);
    agoo_text_ref(emptyResp);
  }
  agoo_res_message_push(req->res, emptyResp);
}

static int user_off = 6;

static void user_handler(agooReq req) {
  agooText t = agoo_respond(200, req->path.start + user_off,
                            req->path.len - user_off, NULL);

  agoo_res_message_push(req->res, t);
}

int main(int argc, char **argv) {
  struct _agooErr err = AGOO_ERR_INIT;
  int port = 3000;

  // Adjust this to increase or decrease the number of IO threads. 1.0 seems
  // to work pretty well on a 4 core (8 processor) machine. It is the ratio of
  // thread to processors.
  agoo_io_loop_ratio = 1.0;
  agoo_poll_wait = 0.01;
  
  if (AGOO_ERR_OK != agoo_init(&err, "simple")) {
    printf("Failed to initialize Agoo. %s\n", err.msg);
    return err.code;
  }
  // Set the number of eval threads. No need for more than one in this
  // case. The multiple read/write threads do most of the work and are
  // adjusted automatically.
  agoo_server.thread_cnt = 1;

  agoo_pages_set_root(&err, ".");

  if (AGOO_ERR_OK != agoo_bind_to_port(&err, port)) {
    printf("Failed to bind to port %d. %s\n", port, err.msg);
    return err.code;
  }
  // set up hooks or routes
  if (AGOO_ERR_OK !=
          agoo_add_func_hook(&err, AGOO_GET, "/", empty_handler, true) ||
      AGOO_ERR_OK !=
          agoo_add_func_hook(&err, AGOO_GET, "/user/*", user_handler, true) ||
      AGOO_ERR_OK !=
          agoo_add_func_hook(&err, AGOO_POST, "/user", empty_handler, true)) {
    return err.code;
  }
  // start the server and wait for it to be shutdown
  if (AGOO_ERR_OK != agoo_start(&err, AGOO_VERSION)) {
    printf("%s\n", err.msg);
    return err.code;
  }
  return 0;
}
