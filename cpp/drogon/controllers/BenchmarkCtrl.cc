#include "BenchmarkCtrl.h"
void BenchmarkCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    auto resp = HttpResponse::newHttpResponse();
    resp->setExpiredTime(0);
    callback(resp);
}