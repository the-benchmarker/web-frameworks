/* Direct C STL HTTP server benchmark, adapted from the C STL HTTP example.
 * No Ruby / Iodine dependency. */
#define FIO_EVERYTHING
#include "fio-stl/include.h"

/* Root route: only GET / is part of the benchmark. */
static void respond_root(fio_http_s *h) {
  fio_str_info_s path = fio_http_path(h);
  /* HTTP request paths start with '/'; only '/' has length 1. */
  if (path.len == 1 && fio_http_method_is_get(h)) {
    fio_http_finish(h);
    return;
  }
  fio_http_send_error_response(h, 404);
}

/* /user route: match the benchmark methods exactly.
 * fio_http_resource_action classifies GET /user/new as a special NEW action
 * and groups PUT/PATCH with POST, so it does not fit this route contract. */
static void respond_user(fio_http_s *h) {
  fio_str_info_s path = fio_http_path(h);
  /* Routing exposes /user as '/' and /user/:id as '/:id'. */
  if (fio_http_method_is_post(h) && path.len == 1) {
    fio_http_finish(h);
    return;
  }
  if (fio_http_method_is_get(h) && path.len > 1) {
    /* Copy the request-owned path slice before the callback returns. */
    fio_http_write(h,
                   .buf = path.buf + 1,
                   .len = path.len - 1,
                   .copy = 1,
                   .finish = 1);
    return;
  }
  fio_http_send_error_response(h, 404);
}

int main(void) {
  fio_http_listener_s *listener =
      fio_http_listen("0.0.0.0:3000", .on_http = respond_root);
  if (!listener || fio_http_route(listener, "/user", .on_http = respond_user))
    return 1;
  fio_io_start(1);
  return 0;
}
