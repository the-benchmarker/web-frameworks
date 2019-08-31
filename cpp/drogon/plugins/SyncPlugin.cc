/**
 *
 *  SyncPlugin.cc
 *
 */

#include "SyncPlugin.h"
#include <drogon/drogon.h>

using namespace drogon;

void SyncPlugin::initAndStart(const Json::Value &config)
{
    /// Initialize and start the plugin
    drogon::app().registerSyncAdvice(
        [](const HttpRequestPtr &req) -> HttpResponsePtr {
            if (req->method() != Get || req->path().length() != 1)
            {
                return HttpResponsePtr{};
            }
            if (req->path() == "/")
            {
                auto resp = HttpResponse::newHttpResponse();
                resp->setContentTypeCodeAndCustomString(
                    CT_TEXT_PLAIN, "Content-Type: text/plain\r\n");
                return resp;
            }
            else
                return HttpResponsePtr{};
        });
}

void SyncPlugin::shutdown()
{
    /// Shutdown the plugin
}
