#include <iostream>
#include <nawa/RequestHandlers/RequestHandler.h>
#include <nawa/Exception.h>
#include <nawa/Connection.h>

using namespace nawa;
using namespace std;

int main(int argc, char const *argv[]) {
	
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
    catch (const Exception &e) {
        cerr << "Could not read config.ini file: " << e.getMessage() << endl;
        return -1;
    }

    // function which handles the requests
    auto handlingFunction = [](Connection& connection) -> int {

        auto requestPath = connection.request.env.getRequestPath();

        if (requestPath.size() == 2 && requestPath[0] == "user") {
            connection.setBody(requestPath[1]);
            return 0;
        } else if (requestPath.size() == 1 && requestPath[0] == "user" 
			&& connection.request.env["REQUEST_METHOD"] == "POST") {
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
        requestHandler = RequestHandler::newRequestHandler(handlingFunction, config, 8);
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
