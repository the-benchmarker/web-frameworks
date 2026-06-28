#pragma once

#include <drogon/HttpController.h>
#include <drogon/HttpResponse.h>
#include <drogon/HttpRequest.h>
#include <json/json.h>

namespace drogon {

/**
 * @brief User Controller for handling user-related HTTP requests.
 * 
 * This controller provides endpoints for user management operations.
 * Implements RESTful API best practices with proper error handling
 * and content negotiation.
 */
class UserCtrl : public drogon::HttpController<UserCtrl>
{
public:
    // Constructor
    UserCtrl() = default;
    
    // Destructor - virtual for proper inheritance
    virtual ~UserCtrl() = default;
    
    // Disable copy and move operations for singleton-like behavior
    UserCtrl(const UserCtrl&) = delete;
    UserCtrl& operator=(const UserCtrl&) = delete;
    UserCtrl(UserCtrl&&) = delete;
    UserCtrl& operator=(UserCtrl&&) = delete;

    METHOD_LIST_BEGIN
    // GET /user/{userId} - Retrieve user by ID
    ADD_METHOD_TO(UserCtrl::getUser, "/user/{1}", Get, "PathParam", "id");
    
    // POST /user - Create a new user
    ADD_METHOD_TO(UserCtrl::addUser, "/user", Post);
    
    METHOD_LIST_END

    /**
     * @brief Get user by ID
     * 
     * @param req HTTP request object
     * @param callback Callback function for async response
     * @param userId User ID as path parameter
     */
    void getUser(
        const HttpRequestPtr& req,
        std::function<void(const HttpResponsePtr&)>&& callback,
        const std::string& userId
    );

    /**
     * @brief Add a new user
     * 
     * @param req HTTP request object containing user data in JSON format
     * @param callback Callback function for async response
     */
    void addUser(
        const HttpRequestPtr& req,
        std::function<void(const HttpResponsePtr&)>&& callback
    );

private:
    /**
     * @brief Create a standardized JSON error response
     * 
     * @param statusCode HTTP status code
     * @param message Error message
     * @return HttpResponsePtr Error response
     */
    HttpResponsePtr createErrorResponse(
        HttpStatusCode statusCode,
        const std::string& message
    );

    /**
     * @brief Create a standardized JSON success response
     * 
     * @param statusCode HTTP status code
     * @param data JSON data to include in response
     * @return HttpResponsePtr Success response
     */
    HttpResponsePtr createSuccessResponse(
        HttpStatusCode statusCode,
        const Json::Value& data
    );

    /**
     * @brief Validate user ID format
     * 
     * @param userId User ID to validate
     * @return bool True if valid, false otherwise
     */
    bool validateUserId(const std::string& userId);

    /**
     * @brief Validate user data from request
     * 
     * @param req HTTP request object
     * @param userData Output parameter for parsed user data
     * @return bool True if valid, false otherwise
     */
    bool validateUserData(
        const HttpRequestPtr& req,
        Json::Value& userData
    );
};

} // namespace drogon
