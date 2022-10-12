#include "hv/hv.h"
#include "hv/HttpServer.h"

int main(int argc, char *argv[])
{
    HttpService service;
    service.base_url = "/";
    service.GET("", [](HttpRequest* req, HttpResponse* res) {
        res->body = "";
        return 200;
    });
    service.GET("user/:id", [](HttpRequest* req, HttpResponse* res) {
        res->body = req->query_params.find("id")->second;
        return 200;
    });
    service.POST("user", [](HttpRequest* req, HttpResponse* res) {
        res->body = "";
        return 200;
    });

    http_server_t server;
    server.port = 8888;
    server.service = &service;
    http_server_run(&server);
}
