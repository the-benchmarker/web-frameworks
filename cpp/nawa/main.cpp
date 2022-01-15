#include <iostream>
#include <nawa/Exception.h>
#include <nawa/RequestHandler/RequestHandler.h>
#include <nawa/connection/Connection.h>
#include <thread>

using namespace nawa;
using namespace std;

int main(int argc, char const* argv[]) {
    
    // Please note that this app uses nawa as a library, it can also be used as an IoC-style
    // framework, making it even easier to write and run apps, see https://github.com/jatofg/nawa

    if (argc < 2) {
        std::cout << "please provide the config file name" << std::endl;
        return -1;
    }

    // Load the config from the given ini file
    Config config;
    try {
        config.read(argv[1]);
    }
    catch (Exception const& e) {
        cerr << "Could not read config.ini file: " << e.getMessage() << endl;
        return -1;
    }

    // function which handles the requests
    auto handlingFunction = [](Connection& connection) -> int {

        auto& env = connection.request().env();
        auto requestPath = env.getRequestPath();

        if (requestPath.size() == 2 && requestPath[0] == "user") {
            connection.setResponseBody(requestPath[1]);
            return 0;
        } else if (requestPath.size() == 1 && requestPath[0] == "user" 
            && env["REQUEST_METHOD"] == "POST") {
            return 0;
        } else if (requestPath.size() == 0) {
            return 0;
        }
        
        connection.setStatus(400);
        return 0;

    };

    // set up the NAWA request handler
    unique_ptr<RequestHandler> requestHandler;
    try {
        // The last argument is the concurrency, which is the number of worker threads for processing requests
        
        requestHandler = RequestHandler::newRequestHandler(handlingFunction, config, thread::hardware_concurrency());
    }
    catch (const Exception &e) {
        cerr << "NAWA request handler could not be created: " << e.getMessage() << endl;
        return -1;
    }

    // start handling requests
    try {
        requestHandler->start();
    } catch (const Exception &e) {
        cerr << "NAWA request handling could not be started: " << e.getMessage() << endl;
        return -1;
    }

    // block until request handling has terminated
    requestHandler->join();

    return 0;
}
