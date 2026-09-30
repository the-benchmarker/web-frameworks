import vibe.core.core : runApplication, runWorkerTaskDist, setupWorkerThreads;
import vibe.http.server;
import vibe.http.router;

import std.parallelism: totalCPUs;

void handleGetUser(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    res.writeBody(req.params["id"], "text/plain");
}

void handlePostUser(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    res.writeBody("", "text/plain");
}

void handleRoot(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
    res.writeBody("", "text/plain");
}

void main()
{
    setupWorkerThreads(totalCPUs);
    runWorkerTaskDist(() nothrow {
		try {
		    auto settings = new HTTPServerSettings;
                settings.port = 3000;
                settings.bindAddresses = ["0.0.0.0"];
                settings.options |= HTTPServerOption.reusePort;

                auto router = new URLRouter;

                router
                    .post("/user", &handlePostUser)
                    .get("/user/:id", &handleGetUser)
                    .get("/", &handleRoot);

                router.rebuild();
                listenHTTP(settings, router);
		} catch (Exception e) assert(false, e.msg);
	});
	runApplication();
}
