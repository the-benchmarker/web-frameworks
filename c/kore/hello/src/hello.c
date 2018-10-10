#include <kore/kore.h>
#include <kore/http.h>

int		home(struct http_request *);
int		user(struct http_request *);
int		user_details(struct http_request *);

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
	char		*id;
	size_t		len;
	struct kore_buf		*buf;
	u_int8_t		*data;

	http_populate_get(req);

	buf = kore_buf_alloc(1024);

	if (http_argument_get_string(req, "id", &id))
		kore_buf_appendf(buf, id);

	data = kore_buf_release(buf, &len);

	http_response(req, 200, data, len);
	kore_free(data);

	return (KORE_RESULT_OK);
}
