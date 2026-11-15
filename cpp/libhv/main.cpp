#include <thread>

#include "hv/HttpServer.h"
#include "hv/hlog.h"

using namespace hv;

int main() {
    hlog_disable();

    HttpService router;
    router.enable_access_log = 0;

    router.GET("/", [](HttpRequest* req, HttpResponse* resp) {
        return 200;
    });

    router.GET("/user/{id}", [](HttpRequest* req, HttpResponse* resp) {
        return resp->String(req->GetParam("id"));
    });

    router.POST("/user", [](HttpRequest* req, HttpResponse* resp) {
        return 200;
    });

    HttpServer server;
    server.registerHttpService(&router);
    server.setHost("0.0.0.0");
    server.setPort(3000);
    server.setThreadNum(std::thread::hardware_concurrency());
    return server.run();
}
