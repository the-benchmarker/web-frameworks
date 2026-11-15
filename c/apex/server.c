#include <apex.h>

static void handle_root(apex_req_t *req, apex_res_t *res) {
    (void)req;
    apex_res_send(res, 200, NULL, 0);
}

static void handle_user_id(apex_req_t *req, apex_res_t *res) {
    apex_res_send(res, 200, req->param, req->param_len);
}

static void handle_user_post(apex_req_t *req, apex_res_t *res) {
    (void)req;
    apex_res_send(res, 200, NULL, 0);
}

int main(void) {
    apex_app_t *app = apex_create();
    if (!app) {
        return 1;
    }

    apex_get(app, "/", handle_root);
    apex_get(app, "/user/:id", handle_user_id);
    apex_post(app, "/user", handle_user_post);

    int status = apex_run(app, 3000);
    apex_destroy(app);
    return status;
}
