#include "MyController.hpp"

// Implementation of MyController validation methods

namespace oatpp {

// Validate user request DTO
bool MyController::validateUserRequest(const oatpp::Object<UserRequest>& userRequest) {
    if (!userRequest) {
        return false;
    }
    
    // Check required fields
    if (userRequest->username.empty()) {
        return false;
    }
    
    // Validate username length (1-50 characters)
    if (userRequest->username->size() > 50) {
        return false;
    }
    
    // If email is provided, validate it (basic check)
    if (userRequest->email && !userRequest->email->empty()) {
        if (userRequest->email->size() > 255) {
            return false;
        }
        
        // Basic email format validation (contains @)
        if (userRequest->email->find('@') == oatpp::String::npos) {
            return false;
        }
    }
    
    return true;
}

} // namespace oatpp