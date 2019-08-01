#include "BenchmarkCtrl.h"
void BenchmarkCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    auto resp = HttpResponse::newHttpResponse();
    resp->setContentTypeCodeAndCustomString(CT_TEXT_PLAIN,
                                            "Content-Type: text/plain\r\n");
    resp->setExpiredTime(0);
    callback(resp);
}
