/**
 * @file SyncPlugin.cc
 * @brief Synchronous Plugin Implementation for Drogon Framework
 */

#include "SyncPlugin.h"
#include <drogon/drogon.h>
#include <drogon/utils.h>

namespace drogon {

bool SyncPlugin::validateRequest(const HttpRequestPtr& req) {
    try {
        // Validate HTTP method
        if (req->method() != Get) {
            return false;
        }
        
        // Validate path length (prevent potential DoS with very long paths)
        if (req->path().length() > 1024) {
            LOG_WARN << "Request path too long: " << req->path().length();
            return false;
        }
        
        // Validate path characters (basic security check)
        const std::string& path = req->path();
        for (char c : path) {
            if (!(isalnum(c) || c == '/' || c == '-' || c == '_' || c == '.' || c == '?')) {
                LOG_WARN << "Invalid character in path: " << c;
                return false;
            }
        }
        
        return true;
        
    } catch (const std::exception& e) {
        LOG_ERROR << "Request validation failed: " << e.what();
        return false;
    }
}

HttpResponsePtr SyncPlugin::createHealthCheckResponse() {
    auto resp = HttpResponse::newHttpJsonResponse();
    resp->setStatusCode(HttpStatusCode::k200OK);
    
    Json::Value healthData;
    healthData["status"] = "healthy";
    healthData["timestamp"] = drogon::utils::getCurrentTimeStr();
    healthData["service"] = "drogon-sync-plugin";
    healthData["version"] = "1.0.0";
    
    // Add system information
    healthData["system"]["uptime"] = drogon::utils::getUptime();
    
    resp->setBody(drogon::utils::toJsonString(healthData));
    resp->addHeader("Content-Type", "application/json; charset=utf-8");
    resp->addHeader("Cache-Control", "no-cache, no-store, must-revalidate");
    resp->addHeader("X-Content-Type-Options", "nosniff");
    resp->addHeader("X-Frame-Options", "DENY");
    resp->addHeader("X-XSS-Protection", "1; mode=block");
    
    return resp;
}

HttpResponsePtr SyncPlugin::handleSyncRequest(const HttpRequestPtr& req) {
    try {
        // Validate request first
        if (!validateRequest(req)) {
            return HttpResponsePtr{nullptr};
        }
        
        // Handle root path for health check
        if (req->path() == "/") {
            return createHealthCheckResponse();
        }
        
        // Handle /health endpoint explicitly
        if (req->path() == "/health" || req->path() == "/health/") {
            return createHealthCheckResponse();
        }
        
        // Handle /status endpoint
        if (req->path() == "/status" || req->path() == "/status/") {
            auto resp = HttpResponse::newHttpJsonResponse();
            resp->setStatusCode(HttpStatusCode::k200OK);
            
            Json::Value statusData;
            statusData["status"] = "running";
            statusData["message"] = "Drogon Sync Plugin is operational";
            statusData["timestamp"] = drogon::utils::getCurrentTimeStr();
            
            resp->setBody(drogon::utils::toJsonString(statusData));
            resp->addHeader("Content-Type", "application/json; charset=utf-8");
            
            return resp;
        }
        
        // Return null for unhandled paths
        return HttpResponsePtr{nullptr};
        
    } catch (const std::exception& e) {
        LOG_ERROR << "Error handling sync request: " << e.what();
        
        // Create error response
        auto errorResp = HttpResponse::newHttpJsonResponse();
        errorResp->setStatusCode(HttpStatusCode::k500InternalServerError);
        
        Json::Value errorData;
        errorData["error"] = true;
        errorData["message"] = "Internal server error";
        errorData["timestamp"] = drogon::utils::getCurrentTimeStr();
        
        errorResp->setBody(drogon::utils::toJsonString(errorData));
        errorResp->addHeader("Content-Type", "application/json; charset=utf-8");
        
        return errorResp;
    }
}

void SyncPlugin::initAndStart(const Json::Value& config) {
    try {
        LOG_INFO << "Initializing SyncPlugin... ";
        
        // Register synchronous advice handler
        drogon::app().registerSyncAdvice([this](const HttpRequestPtr& req) -> HttpResponsePtr {
            return this->handleSyncRequest(req);
        });
        
        LOG_INFO << "SyncPlugin initialized successfully.";
        
    } catch (const std::exception& e) {
        LOG_ERROR << "Failed to initialize SyncPlugin: " << e.what();
        throw;
    }
}

void SyncPlugin::shutdown() {
    try {
        LOG_INFO << "Shutting down SyncPlugin... ";
        
        // Perform cleanup operations here if needed
        // For now, just log the shutdown
        
        LOG_INFO << "SyncPlugin shutdown completed.";
        
    } catch (const std::exception& e) {
        LOG_ERROR << "Error during SyncPlugin shutdown: " << e.what();
    }
}

} // namespace drogon
