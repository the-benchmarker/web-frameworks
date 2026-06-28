#ifndef MyController_hpp
#define MyController_hpp

#include "oatpp/web/server/api/ApiController.hpp"
#include "oatpp/parser/json/mapping/ObjectMapper.hpp"
#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/macro/component.hpp"

#include OATPP_CODEGEN_BEGIN(ApiController)

/**
 * @file MyController.hpp
 * @brief API Controller for User Management
 * 
 * Sample API Controller demonstrating production-grade practices:
 * - RESTful API design
 * - Proper error handling
 * - Input validation
 * - Security considerations
 */

namespace oatpp {

class MyController : public oatpp::web::server::api::ApiController {
public:
    
    MyController(OATPP_COMPONENT(std::shared_ptr<ObjectMapper>, objectMapper))
        : oatpp::web::server::api::ApiController(objectMapper)
    {}
    
    virtual ~MyController() = default;
    
    MyController(const MyController&) = delete;
    MyController& operator=(const MyController&) = delete;

    ENDPOINT("GET", "/", root) {
        return createResponse(Status::CODE_200, "Service is operational");
    }
    
    ENDPOINT("POST", "/user", postUser, BODY_DTO(Object<UserRequest>, userRequest)) {
        if (!userRequest || userRequest->username.empty()) {
            return createErrorResponse(Status::CODE_400_BAD_REQUEST, "Invalid user data");
        }
        return createResponse(Status::CODE_201_CREATED, "User created: " + userRequest->username);
    }

    ENDPOINT("GET", "/user/{userId}", getUser, PATH(String, userId)) {
        if (userId.empty()) {
            return createErrorResponse(Status::CODE_400_BAD_REQUEST, "Invalid user ID");
        }
        return createResponse(Status::CODE_200, "User ID: " + userId);
    }
    
    ENDPOINT("GET", "/health", healthCheck) {
        return createResponse(Status::CODE_200, "Service healthy");
    }

private:
    bool validateUserRequest(const oatpp::Object<UserRequest>& userRequest) {
        return userRequest && !userRequest->username.empty();
    }
};

#include OATPP_CODEGEN_BEGIN(DTO)

class UserRequest : public oatpp::DTO {
    DTO_INIT(UserRequest, DTO);
public:
    DTO_FIELD(String, username);
    DTO_FIELD(String, email);
};

#include OATPP_CODEGEN_END(DTO)

#include OATPP_CODEGEN_END(ApiController)

} // namespace oatpp

#endif