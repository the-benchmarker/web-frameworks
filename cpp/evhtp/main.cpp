#include <stdio.h>
#include <cstring>
#include <thread>
#include <evhtp.h>

// No error checking or anything of the sort, as per the guidelines

const int num_threads = -1;

void on_request_index(evhtp_request_t *req, void*) {
	if(req->method == htp_method_GET) {
		evhtp_send_reply(req, EVHTP_RES_OK);
	}
}

void on_request_user_register(evhtp_request_t *req, void*) {
	if(req->method == htp_method_POST) {
		evhtp_send_reply(req, EVHTP_RES_OK);
	}
}

void on_request_user_index(evhtp_request_t *req, void*) {
	if(req->method == htp_method_GET) {
		const char *p = req->uri->path->full + 6; // 6 being the length of "/user/"
		const unsigned int pl = strlen(p);
		evbuffer_add_reference(req->buffer_out, p, pl, NULL, NULL);
		evhtp_send_reply(req, EVHTP_RES_OK);
	}
}

int main(int argc, char **argv) {
	evbase_t *evbase = event_base_new();
	evhtp_t *htp     = evhtp_new(evbase, NULL);

	evhtp_set_parser_flags(htp, EVHTP_PARSE_QUERY_FLAG_LENIENT);
	evhtp_enable_flag(htp, EVHTP_FLAG_ENABLE_NODELAY);

	evhtp_set_cb(htp, "/user/", on_request_user_index, NULL);
	evhtp_set_cb(htp, "/user", on_request_user_register, NULL);
	evhtp_set_cb(htp, "/", on_request_index, NULL);
	evhtp_bind_socket(htp, "0.0.0.0", 3000, 1024);

	if(num_threads > 1) {
		evhtp_use_threads_wexit(htp, NULL, NULL, num_threads, NULL);
	}
	else if(num_threads < 0) {
		const int cores = std::thread::hardware_concurrency();
		if(cores > 0 && cores > -num_threads) {
			evhtp_use_threads_wexit(htp, NULL, NULL, cores + num_threads, NULL);
		}
	}

	event_base_loop(evbase, 0);
	return 0;
}

