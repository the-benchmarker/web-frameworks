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
    if (req.method == Request.Method.Get)
    {
        if (req.uri == "/") output.status = 200;
        else if (req.uri.startsWith("/user/")) output ~= req.uri[6..$];
    }
    else if (req.method == Request.Method.Post && req.uri == "/user") output.status = 200;
}
