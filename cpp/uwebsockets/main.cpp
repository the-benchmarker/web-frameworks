#include <iostream>
#include <thread>
#include <vector>

#include "App.h"

/* uWebSockets is single threaded: one isolated App (and event loop) per
 * thread, all listening on the same port (uSockets sets SO_REUSEPORT). */
static void serve() {
    uWS::App()
        .get("/", [](auto *res, auto * /*req*/) {
            res->end();
        })
        .get("/user/:id", [](auto *res, auto *req) {
            res->end(req->getParameter("id"));
        })
        .post("/user", [](auto *res, auto * /*req*/) {
            res->end();
        })
        .listen("0.0.0.0", 3000, [](auto *listen_socket) {
            if (!listen_socket) {
                std::cerr << "Failed to listen on port 3000" << std::endl;
            }
        })
        .run();
}

int main() {
    unsigned int concurrency = std::thread::hardware_concurrency();

    std::vector<std::thread> threads;
    for (unsigned int i = 1; i < concurrency; i++) {
        threads.emplace_back(serve);
    }
    serve();

    for (auto &thread : threads) {
        thread.join();
    }
    return 1;
}
