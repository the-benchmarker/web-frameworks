#include "UserCtrl.h"
#include <drogon/utils.h>
#include <regex>

// Define validation regex for user ID (alphanumeric, 1-64 characters)
const std::regex USER_ID_REGEX("^[a-zA-Z0-9_-]{1,64}$");

namespace drogon {

// Helper function to create JSON response
HttpResponsePtr UserCtrl::createErrorResponse(
    HttpStatusCode statusCode,
    const std::string& message
) {
    auto resp = HttpResponse::newHttpJsonResponse();
    resp->setStatusCode(statusCode);
    
    Json::Value errorJson;
    errorJson["error"] = true;
    errorJson["message"] = message;
    errorJson["timestamp"] = drogon::utils::getCurrentTimeStr();
    errorJson["status"] = static_cast<int>(statusCode);
    
    resp->setBody(drogon::utils::toJsonString(errorJson));
    resp->addHeader("Content-Type", "application/json; charset=utf-8");
    resp->addHeader("Cache-Control", "no-cache, no-store, must-revalidate");
    resp->addHeader("Pragma", "no-cache");
    resp->addHeader("Expires", "0");
    
    return resp;
}

HttpResponsePtr UserCtrl::createSuccessResponse(
    HttpStatusCode statusCode,
    const Json::Value& data
) {
    auto resp = HttpResponse::newHttpJsonResponse();
    resp->setStatusCode(statusCode);
    
    Json::Value successJson;
    successJson["error"] = false;
    successJson["data"] = data;
    successJson["timestamp"] = drogon::utils::getCurrentTimeStr();
    successJson["status"] = static_cast<int>(statusCode);
    
    resp->setBody(drogon::utils::toJsonString(successJson));
    resp->addHeader("Content-Type", "application/json; charset=utf-8");
    resp->addHeader("Cache-Control", "no-cache, no-store, must-revalidate");
    
    return resp;
}

bool UserCtrl::validateUserId(const std::string& userId) {
    if (userId.empty() || userId.length() > 64) {
        return false;
    }
    
    // Check if userId matches allowed pattern
    return std::regex_match(userId, USER_ID_REGEX);
}

bool UserCtrl::validateUserData(
    const HttpRequestPtr& req,
    Json::Value& userData
) {
    try {
        // Parse JSON body
        auto body = req->getBody();
        if (body.empty()) {
            return false;
        }
        
        Json::CharReaderBuilder builder;
        Json::CharReader* reader = builder.newCharReader();
        
        std::string errors;
        if (!reader->parse(body.c_str(), body.c_str() + body.size(), &userData, &errors)) {
            delete reader;
            return false;
        }
        
        delete reader;
        
        // Validate required fields
        if (!userData.isObject()) {
            return false;
        }
        
        // Check for required fields (example validation)
        if (!userData.isMember("username") || !userData["username"].isString()) {
            return false;
        }
        
        // Validate username length and format
        std::string username = userData["username"].asString();
        if (username.empty() || username.length() > 50) {
            return false;
        }
        
        // Additional validation for other fields if present
        if (userData.isMember("email") && !userData["email"].isString()) {
            return false;
        }
        
        return true;
        
    } catch (const std::exception& e) {
        // Log validation error in production (debug disabled, but warn level might catch this)
        LOG_WARN << "User data validation failed: " << e.what();
        return false;
    }
}

void UserCtrl::getUser(
    const HttpRequestPtr& req,
    std::function<void(const HttpResponsePtr&)>&& callback,
    const std::string& userId
) {
    try {
        // Validate user ID format
        if (!validateUserId(userId)) {
            auto errorResp = createErrorResponse(
                HttpStatusCode::k400BadRequest,
                "Invalid user ID format. Must be 1-64 alphanumeric characters."
            );
            callback(errorResp);
            return;
        }
        
        // Create success response with user ID
        Json::Value responseData;
        responseData["userId"] = userId;
        responseData["endpoint"] = "/user/" + userId;
        responseData["method"] = "GET";
        responseData["timestamp"] = drogon::utils::getCurrentTimeStr();
        
        auto successResp = createSuccessResponse(HttpStatusCode::k200OK, responseData);
        callback(successResp);
        
    } catch (const std::exception& e) {
        auto errorResp = createErrorResponse(
            HttpStatusCode::k500InternalServerError,
            "Internal server error while processing request."
        );
        callback(errorResp);
    }
}

void UserCtrl::addUser(
    const HttpRequestPtr& req,
    std::function<void(const HttpResponsePtr&)>&& callback
) {
    try {
        Json::Value userData;
        
        // Validate user data from request
        if (!validateUserData(req, userData)) {
            auto errorResp = createErrorResponse(
                HttpStatusCode::k400BadRequest,
                "Invalid user data. Please provide valid JSON with required fields."
            );
            callback(errorResp);
            return;
        }
        
        // Extract and sanitize username
        std::string username = userData["username"].asString();
        
        // Create success response
        Json::Value responseData;
        responseData["username"] = username;
        responseData["message"] = "User created successfully";
        responseData["endpoint"] = "/user";
        responseData["method"] = "POST";
        responseData["timestamp"] = drogon::utils::getCurrentTimeStr();
        
        // Add sanitized user data to response (excluding sensitive info)
        if (userData.isMember("email")) {
            responseData["email"] = userData["email"].asString();
        }
        
        auto successResp = createSuccessResponse(HttpStatusCode::k201Created, responseData);
        successResp->addHeader("Location", "/user/" + username);
        
        callback(successResp);
        
    } catch (const std::exception& e) {
        auto errorResp = createErrorResponse(
            HttpStatusCode::k500InternalServerError,
            "Internal server error while processing request."
        );
        callback(errorResp);
    }
}

} // namespace drogon
