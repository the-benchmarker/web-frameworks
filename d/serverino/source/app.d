module app;

import serverino;
import std.datetime: seconds;
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
        .setWorkers(2)
        .enableWorkerBacklog(16);
}

@safe @endpoint @route!"/"
void index(Request req, Output output) {
    if (req.method == Request.Method.Get)
        output.status = 200;
}

@safe @endpoint @route!"/user"
void createUser(Request req, Output output) {
    if (req.method == Request.Method.Post)
        output.status = 200;
}

@safe @endpoint @route!(r => r.path.startsWith("/user/"))
void getUser(Request req, Output output) {
    if (req.method == Request.Method.Get)
        output ~= req.path[6..$];
}
