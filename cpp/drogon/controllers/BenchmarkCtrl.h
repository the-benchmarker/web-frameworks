#pragma once
#include <drogon/HttpSimpleController.h>
using namespace drogon;
class BenchmarkCtrl : public drogon::HttpSimpleController<BenchmarkCtrl>
{
  public:
    virtual void asyncHandleHttpRequest(
        const HttpRequestPtr &req,
        std::function<void(const HttpResponsePtr &)> &&callback) override;
    PATH_LIST_BEGIN
    // list path definitions here;
    PATH_ADD("/", Get);
    PATH_LIST_END
};
