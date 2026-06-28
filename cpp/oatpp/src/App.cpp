#include "./controller/MyController.hpp"
#include "./AppComponent.hpp"

#include "oatpp/network/Server.hpp"
#include "oatpp/core/async/ThreadPool.hpp"
#include "oatpp/core/base/Environment.hpp"
#include "oatpp/web/protocol/http/outgoing/BufferBody.hpp"

#include <iostream>
#include <csignal>
#include <atomic>
#include <thread>

// Global flag for graceful shutdown
std::atomic<bool> g_shutdown_flag{false};

// Signal handler for graceful shutdown
void signal_handler(int signal) {
    if (!g_shutdown_flag.load()) {
        std::cout << "Received signal " << signal << ". Shutting down gracefully..." << std::endl;
        g_shutdown_flag.store(true);
    }
}

// Setup signal handlers for production
void setup_signal_handlers() {
    struct sigaction sa;
    sa.sa_handler = signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    
    // Handle common shutdown signals
    sigaction(SIGTERM, &sa, nullptr);
    sigaction(SIGINT, &sa, nullptr);
    sigaction(SIGQUIT, &sa, nullptr);
    
    // Ignore pipe errors (common in HTTP servers)
    signal(SIGPIPE, SIG_IGN);
}

// Custom error handler for production
void customErrorHandler(
    std::v1::oatpp::String errorMessage,
    std::v1::oatpp::String errorDescription,
    oatpp::base::ErrorCode errorCode
) {
    // In production, you might want to log to a file or monitoring system
    // For now, we'll output to stderr but with controlled formatting
    std::cerr << "[ERROR] " << errorMessage << " - " << errorDescription << " (Code: " << errorCode << ")" << std::endl;
}

// Configure environment for production
void configure_production_environment() {
    // Set custom error handler
    oatpp::base::Environment::setErrorHandler(customErrorHandler);
    
    // Configure thread pool for production workload
    // Default thread pool might be sufficient, but we can customize it if needed
    
    // Disable console logging in production (can be controlled by build flag)
    #ifdef OATPP_DISABLE_CONSOLE_LOG
    oatpp::base::Environment::setConsoleLogDisabled(true);
    #endif
    
    // Set log level to WARN for production (disable debug logs)
    oatpp::base::Environment::setLogLevel(oatpp::base::Logger::LEVEL_WARN);
}

void run() {
    try {
        // Configure production environment
        configure_production_environment();

        /* Register Components in scope of run() method */
        AppComponent components;

        /* Get router component */
        OATPP_COMPONENT(std::shared_ptr<oatpp::web::server::HttpRouter>, router);

        /* Create MyController and add all of its endpoints to router */
        auto controller = std::make_shared<MyController>();
        router->addController(controller);

        /* Get connection handler component */
        OATPP_COMPONENT(std::shared_ptr<oatpp::network::ConnectionHandler>, connectionHandler);

        /* Get connection provider component */
        OATPP_COMPONENT(std::shared_ptr<oatpp::network::ServerConnectionProvider>, connectionProvider);

        // Configure connection provider for production
        // Set reasonable limits to prevent resource exhaustion
        connectionProvider->setMaxConnections(10000);
        connectionProvider->setConnectionTimeout(5000); // 5 seconds
        connectionProvider->setMaxConnectionAttempts(3);

        /* Create server which takes provided TCP connections and passes them to HTTP connection handler */
        oatpp::network::Server server(connectionProvider, connectionHandler);

        // Configure server for production
        server.setErrorHandler([](const std::exception_ptr& exception) {
            try {
                std::rethrow_exception(exception);
            } catch (const std::exception& e) {
                std::cerr << "[SERVER ERROR] " << e.what() << std::endl;
            } catch (...) {
                std::cerr << "[SERVER ERROR] Unknown exception" << std::endl;
            }
        });

        // Run server in a separate thread for graceful shutdown
        std::thread server_thread([&server]() {
            try {
                server.run();
            } catch (const std::exception& e) {
                std::cerr << "[SERVER ERROR] " << e.what() << std::endl;
                g_shutdown_flag.store(true);
            } catch (...) {
                std::cerr << "[SERVER ERROR] Unknown exception" << std::endl;
                g_shutdown_flag.store(true);
            }
        });

        // Wait for shutdown signal
        while (!g_shutdown_flag.load()) {
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }

        // Stop the server gracefully
        server.stop();
        
        // Join the server thread
        if (server_thread.joinable()) {
            server_thread.join();
        }
        
    } catch (const std::exception& e) {
        std::cerr << "[FATAL ERROR] " << e.what() << std::endl;
        throw;
    } catch (...) {
        std::cerr << "[FATAL ERROR] Unknown exception" << std::endl;
        throw;
    }
}

/**
 * @brief Main entry point
 * 
 * Initializes the application, sets up signal handlers,
 * and starts the HTTP server.
 */
int main(int argc, const char * argv[]) {
    try {
        // Set up signal handlers for production-grade graceful shutdown
        setup_signal_handlers();
        
        // Initialize oatpp environment
        oatpp::base::Environment::init();

        // Run the application
        run();
        
        // Cleanup oatpp environment
        oatpp::base::Environment::destroy();
        
        return EXIT_SUCCESS;
        
    } catch (const std::exception& e) {
        std::cerr << "[FATAL] Application failed: " << e.what() << std::endl;
        oatpp::base::Environment::destroy();
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "[FATAL] Application failed with unknown error" << std::endl;
        oatpp::base::Environment::destroy();
        return EXIT_FAILURE;
    }
}
