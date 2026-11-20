#pragma once
#include <drogon/HttpController.h>
using namespace drogon;
class UserCtrl : public drogon::HttpController<UserCtrl>
{
  public:
    METHOD_LIST_BEGIN
    ADD_METHOD_TO(UserCtrl::getUser, "/user/{1}", Get);
    ADD_METHOD_TO(UserCtrl::addUser, "/user", Post);

    METHOD_LIST_END
    void getUser(const HttpRequestPtr &req,
                 std::function<void(const HttpResponsePtr &)> &&callback,
                 std::string &&id);
    void addUser(const HttpRequestPtr &req,
                 std::function<void(const HttpResponsePtr &)> &&callback);
};
