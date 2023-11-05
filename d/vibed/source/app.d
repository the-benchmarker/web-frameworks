import vibe.core.core : runApplication, runWorkerTask, setupWorkerThreads;
import vibe.http.server;
import std.parallelism: totalCPUs;
import std.algorithm: startsWith;

void handleRequest(scope HTTPServerRequest req, scope HTTPServerResponse res)
{
	if (req.requestURI == "/")
		res.writeBody("", "text/plain");
    else if (startsWith(req.requestURI,"/user")) {
        if (req.method == HTTPMethod.POST)
		    res.writeBody("", "text/plain");
        else if (req.method == HTTPMethod.GET)
            res.writeBody(req.requestURI[6..$], "text/plain");
    }
}

void main()
{
    setupWorkerThreads(2*totalCPUs);
    runWorkerTask(&runServer);
	runApplication();
}

void runServer() nothrow
{
    try {
        auto settings = new HTTPServerSettings;
        settings.options |= HTTPServerOption.reusePort;
        settings.port = 3000;
        settings.bindAddresses = ["0.0.0.0"];
        listenHTTP(settings, &handleRequest);
    } catch (Exception e) assert(false, e.msg);
}
