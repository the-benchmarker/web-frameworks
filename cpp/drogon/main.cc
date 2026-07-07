#include <drogon/drogon.h>
#include <iostream>
#include <filesystem>
#include <signal.h>
#include <unistd.h>

// Global flag for graceful shutdown
volatile sig_atomic_t g_shutdown_flag = 0;

// Signal handler for graceful shutdown
void signal_handler(int signal) {
    g_shutdown_flag = 1;
    drogon::app().getLoop()->queueInLoop([&]() {
        LOG_INFO("Received signal ", signal, ". Shutting down gracefully...");
        drogon::app().quit();
    });
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

// Validate config file exists and is readable
bool validate_config_file(const std::string& config_path) {
    std::filesystem::path path(config_path);
    
    if (!std::filesystem::exists(path)) {
        std::cerr << "Error: Config file '" << config_path << "' does not exist." << std::endl;
        return false;
    }
    
    if (!std::filesystem::is_regular_file(path)) {
        std::cerr << "Error: Config file '" << config_path << "' is not a regular file." << std::endl;
        return false;
    }
    
    if (std::filesystem::file_size(path) == 0) {
        std::cerr << "Error: Config file '" << config_path << "' is empty." << std::endl;
        return false;
    }
    
    return true;
}

int main(int argc, char const *argv[]) {
    // Set up signal handlers for production-grade graceful shutdown
    setup_signal_handlers();
    
    // Validate command line arguments
    if (argc < 2) {
        std::cerr << "Error: Please provide the config file name." << std::endl;
        std::cerr << "Usage: " << argv[0] << " <config_file>" << std::endl;
        return EXIT_FAILURE;
    }
    
    // Validate config file
    if (!validate_config_file(argv[1])) {
        return EXIT_FAILURE;
    }
    
    try {
        // Initialize and run drogon application
        // Set log level to WARN for production (disable debug logs)
        drogon::app().setLogLevel(drogon::Warn);
        
        // Disable server header for security (already in config, but explicit here)
        drogon::app().setServerHeaderEnabled(false);
        
        // Disable date header for minimal response size
        drogon::app().setDateHeaderEnabled(false);
        
        // Load configuration and run
        drogon::app().loadConfigFile(argv[1]).run();
        
        return EXIT_SUCCESS;
    } catch (const std::exception& e) {
        std::cerr << "Fatal error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    } catch (...) {
        std::cerr << "Unknown fatal error occurred." << std::endl;
        return EXIT_FAILURE;
    }
}
