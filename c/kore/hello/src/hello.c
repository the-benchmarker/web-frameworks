#include <kore/http.h>
#include <kore/kore.h>

int home(struct http_request *req);
int user(struct http_request *req);
int user_details(struct http_request *req);

int
home(struct http_request *req)
{
    http_response(req, 200, NULL, 0);
    return (KORE_RESULT_OK);
}

int
user(struct http_request *req)
{
    http_response(req, 200, NULL, 0);
    return (KORE_RESULT_OK);
}

int
user_details(struct http_request *req)
{
    size_t      nmatch = 2;

    char        *id;
    struct      kore_buf     *buf;
    regex_t     re;
    regmatch_t  pmatch[nmatch];

    buf = kore_buf_alloc(1024);

    //kore_log(LOG_NOTICE, "path: %s", req->path);
    //kore_log(LOG_NOTICE, "hdlr path: %s", req->hdlr->path);

    if (regcomp(&re, req->hdlr->path, REG_EXTENDED)) {
        kore_debug("regcomp() on %s failed", req->path);
        kore_buf_free(buf);
        regfree(&re);

        return (KORE_RESULT_ERROR);
    }

    if (regexec(&re, req->path, nmatch, pmatch, 0) != 0) {
        kore_debug("regexec() on %s failed", req->path);
        kore_buf_free(buf);
        regfree(&re);

        return (KORE_RESULT_ERROR);
    }

    //id = strndup(req->path + pmatch[1].rm_so, pmatch[1].rm_eo - pmatch[1].rm_so);
    unsigned int id_len = pmatch[1].rm_eo - pmatch[1].rm_so + 1;
    id = kore_malloc(id_len);
    kore_strlcpy(id, req->path + pmatch[1].rm_so, id_len);

    kore_buf_appendf(buf, id);

    http_response(req, 200, buf->data, buf->offset);
    kore_buf_free(buf);
    // Manually free `id` if `strndup` is used above.
    //free(id);
    regfree(&re);

    return (KORE_RESULT_OK);
}
