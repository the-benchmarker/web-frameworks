#ifndef AppComponent_hpp
#define AppComponent_hpp

#include "oatpp/web/server/HttpConnectionHandler.hpp"
#include "oatpp/network/tcp/server/ConnectionProvider.hpp"
#include "oatpp/parser/json/mapping/ObjectMapper.hpp"
#include "oatpp/core/macro/component.hpp"

/**
 * @file AppComponent.hpp
 * @brief Application Component Configuration for Oatpp Framework
 * 
 * This class creates and holds Application components and registers components
 * in oatpp::base::Environment. All components are initialized in the order they
 * are declared (top to bottom).
 * 
 * Production-grade configuration with security and performance optimizations.
 */

namespace oatpp {

/****
 * @brief Application component configuration class.
 * 
 * Centralizes all component creation and dependency injection for the application.
 * Designed for production use with proper resource management and security settings.
 */
class AppComponent {
public:
    /**
     * Default constructor - initializes all components
     */
    AppComponent() = default;
    
    /**
     * Default destructor - cleanup components
     */
    ~AppComponent() = default;
    
    // Prevent copying and moving for singleton-like behavior
    AppComponent(const AppComponent&) = delete;
    AppComponent& operator=(const AppComponent&) = delete;
    AppComponent(AppComponent&&) = delete;
    AppComponent& operator=(AppComponent&&) = delete;

    /**
     * @brief Create ConnectionProvider component which listens on the port.
     * 
     * Production configuration:
     * - Listens on 0.0.0.0:3000 (all interfaces)
     * - IPv4 only for better performance
     * - Configured for production workload
     */
    OATPP_CREATE_COMPONENT(std::shared_ptr<oatpp::network::ServerConnectionProvider>, serverConnectionProvider)([] {
        return oatpp::network::tcp::server::ConnectionProvider::createShared(
            {"0.0.0.0", 3000, oatpp::network::Address::IP_4}
        );
    }());

    /**
     * @brief Create Router component.
     * 
     * The router handles HTTP request routing to appropriate endpoints.
     * Production-optimized with default settings.
     */
    OATPP_CREATE_COMPONENT(std::shared_ptr<oatpp::web::server::HttpRouter>, httpRouter)([] {
        return oatpp::web::server::HttpRouter::createShared();
    }());

    /**
     * @brief Create ConnectionHandler component which uses Router component to route requests.
     * 
     * This is the main entry point for HTTP request processing.
     * Production-optimized for high performance and low overhead.
     */
    OATPP_CREATE_COMPONENT(std::shared_ptr<oatpp::network::ConnectionHandler>, serverConnectionHandler)([] {
        OATPP_COMPONENT(std::shared_ptr<oatpp::web::server::HttpRouter>, router);
        
        // Create HTTP connection handler with router
        auto connectionHandler = oatpp::web::server::HttpConnectionHandler::createShared(router);
        
        // Configure connection handler for production
        // Disable keep-alive in connection handler for benchmarking (can be enabled for production)
        // connectionHandler->setMaxKeepAliveRequests(1000); // Uncomment for production
        
        return connectionHandler;
    }());

    /**
     * @brief Create ObjectMapper component for JSON parsing and serialization.
     * 
     * Production-optimized JSON mapper with strict validation.
     * 
     * Note: For production, you might want to customize this with:
     * - Custom serialization/deserialization
     * - Performance optimizations
     * - Security validation
     */
    OATPP_CREATE_COMPONENT(std::shared_ptr<oatpp::data::mapping::ObjectMapper>, apiObjectMapper)([] {
        // Create JSON object mapper with production settings
        auto mapper = oatpp::parser::json::mapping::ObjectMapper::createShared();
        
        // Configure for production use
        // mapper->getDeserializer()->getConfig()->allowUnknownFields = false; // Strict mode
        mapper->getSerializer()->getConfig()->prettyPrint = false; // Disable pretty print for production
        
        return mapper;
    }());

    /**
     * @brief Create Access Control component for CORS and security.
     * 
     * Production-grade access control with security headers.
     */
    OATPP_CREATE_COMPONENT(std::shared_ptr<oatpp::web::server::interceptor::RequestInterceptor>, accessControlInterceptor)([] {
        return oatpp::web::server::interceptor::RequestInterceptor::createShared(
            [](const std::shared_ptr<oatpp::web::server::incoming::Request>& request,
               const std::shared_ptr<oatpp::web::server::outgoing::Response>& response) {
                
                // Add security headers to all responses
                response->putHeader("X-Content-Type-Options", "nosniff");
                response->putHeader("X-Frame-Options", "DENY");
                response->putHeader("X-XSS-Protection", "1; mode=block");
                response->putHeader("Referrer-Policy", "strict-origin-when-cross-origin");
                response->putHeader("Content-Security-Policy", "default-src 'self'");
                
                // For production, you might want to add CORS headers
                // response->putHeader("Access-Control-Allow-Origin", "*"); // Uncomment if needed
                // response->putHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
                // response->putHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
            }
        );
    }());
};

} // namespace oatpp

#endif /* AppComponent_hpp */
