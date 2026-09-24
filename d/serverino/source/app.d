module app;

import serverino;
import std.datetime: Duration, seconds;
import std.array: split;
import std.algorithm: startsWith, max;
import std.parallelism: totalCPUs;

mixin ServerinoMain;

@onServerInit ServerinoConfig configure()
{
    return ServerinoConfig
        .create()
        .setHttpTimeout(10.seconds)
        .enableKeepAlive(180.seconds)
        .addListener("0.0.0.0", 3000)
        .setDaemonInstances(max(1, totalCPUs * 3 / 4))
        .setWorkers(1)
        .enableWorkerBacklog(16);
}

@safe
@endpoint void hello(Request req, Output output) {
    if (req.path == "/" && req.method == Request.Method.Get)
        output.status = 200;
    else if (req.path == "/user" && req.method == Request.Method.Post)
        output.status = 200;
    else if (req.path.startsWith("/user/") && req.method == Request.Method.Get)
        output ~= req.path[6..$];
}
