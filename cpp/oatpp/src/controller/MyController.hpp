#ifndef MyController_hpp
#define MyController_hpp

#include "oatpp/web/server/api/ApiController.hpp"
#include "oatpp/core/macro/codegen.hpp"
#include "oatpp/core/macro/component.hpp"

#include OATPP_CODEGEN_BEGIN(ApiController) //<-- Begin Codegen

/**
 * Sample Api Controller.
 */
class MyController : public oatpp::web::server::api::ApiController {
public:
  
   MyController(OATPP_COMPONENT(std::shared_ptr<ObjectMapper>, objectMapper) /* Inject object mapper */)
    : oatpp::web::server::api::ApiController(objectMapper)
  {}

  ENDPOINT("GET", "/", root) {
    return createResponse(Status::CODE_200, "");
  }
  
  ENDPOINT("POST", "/user", postUser) {
    return createResponse(Status::CODE_200, "");
  }

  ENDPOINT("GET", "/user/{userId}", getUser,
          PATH(String, userId)) {
    return createResponse(Status::CODE_200, userId);
  }
  
};

#include OATPP_CODEGEN_END(ApiController) //<-- End Codegen

#endif /* MyController_hpp */
