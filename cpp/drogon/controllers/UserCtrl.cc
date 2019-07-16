#include "UserCtrl.h"
// add definition of your processing function here
void UserCtrl::getUser(const HttpRequestPtr &req,
                       std::function<void(const HttpResponsePtr &)> &&callback,
                       std::string &&id)
{
    auto resp = HttpResponse::newHttpResponse();
    resp->setBody(std::move(id));
    callback(resp);
}
void UserCtrl::addUser(const HttpRequestPtr &req,
                       std::function<void(const HttpResponsePtr &)> &&callback)
{
    auto resp = HttpResponse::newHttpResponse();
    callback(resp);
}