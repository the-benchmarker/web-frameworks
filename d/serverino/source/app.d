module app;

import serverino;
import std.datetime: Duration, seconds;
import std.parallelism: totalCPUs;
import std.array: split;
import std.algorithm: startsWith;

mixin ServerinoMain;

@onServerInit ServerinoConfig configure()
{
	return ServerinoConfig
        .create()
        .setHttpTimeout(10.seconds)
        .enableKeepAlive(180.seconds)
        .addListener("0.0.0.0", 3000)
        .setWorkers(totalCPUs);
}

@safe
@endpoint void hello(Request req, Output output) {
    if (req.uri == "/" && req.method == Request.Method.Get)
        output.status = 200;
    else if (req.uri == "/user" && req.method == Request.Method.Post)
        output.status = 200;
    else if (req.uri.startsWith("/user/") && req.method == Request.Method.Get)
        output ~= req.uri[6..$];
}
