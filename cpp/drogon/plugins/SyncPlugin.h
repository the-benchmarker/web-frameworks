/**
 * @file SyncPlugin.h
 * @brief Synchronous Plugin for Drogon Framework
 * 
 * This plugin provides synchronous request handling capabilities.
 * Implements production-grade error handling and performance optimizations.
 */

#pragma once

#include <drogon/plugins/Plugin.h>
#include <drogon/HttpRequest.h>
#include <drogon/HttpResponse.h>
#include <json/json.h>

namespace drogon {

/**
 * @brief Synchronous Plugin for handling synchronous HTTP requests.
 * 
 * This plugin registers a sync advice handler that processes requests
 * synchronously for specific routes. It's designed for production use
 * with proper error handling, security, and performance considerations.
 */
class SyncPlugin : public drogon::Plugin<SyncPlugin>
{
public:
    /**
     * @brief Constructor
     */
    SyncPlugin() = default;
    
    /**
     * @brief Destructor - virtual for proper inheritance
     */
    virtual ~SyncPlugin() = default;
    
    // Disable copy and move operations
    SyncPlugin(const SyncPlugin&) = delete;
    SyncPlugin& operator=(const SyncPlugin&) = delete;
    SyncPlugin(SyncPlugin&&) = delete;
    SyncPlugin& operator=(SyncPlugin&&) = delete;

    /**
     * @brief Initialize and start the plugin.
     * 
     * This method must be called by drogon to initialize and start the plugin.
     * It registers the synchronous advice handler.
     * 
     * @param config Plugin configuration as JSON
     */
    virtual void initAndStart(const Json::Value& config) override;

    /**
     * @brief Shutdown the plugin.
     * 
     * This method must be called by drogon to shutdown the plugin.
     * Performs cleanup operations.
     */
    virtual void shutdown() override;

private:
    /**
     * @brief Handle synchronous requests for specific routes.
     * 
     * @param req HTTP request object
     * @return HttpResponsePtr Response object or null if not handled
     */
    HttpResponsePtr handleSyncRequest(const HttpRequestPtr& req);

    /**
     * @brief Create a standardized health check response.
     * 
     * @return HttpResponsePtr Health check response
     */
    HttpResponsePtr createHealthCheckResponse();

    /**
     * @brief Validate request for security.
     * 
     * @param req HTTP request object
     * @return bool True if request is valid, false otherwise
     */
    bool validateRequest(const HttpRequestPtr& req);
};

} // namespace drogon

