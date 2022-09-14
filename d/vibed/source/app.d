import vibe.core.core : runApplication;
import vibe.http.router;
import vibe.http.server;
import vibe.web.web;

class Service
{
    @path("/")
    void getIndex()
    {
        status(200);
        response.writeVoidBody();
    }

    @path("/user")
    void postUser()
    {
        status(200);
        response.writeVoidBody();
    }

    @path("/user/:id")
    void getUser(string _id)
    {
        response.writeBody(_id, 200);
    }
}

void main()
{
    auto router = new URLRouter;
    router.registerWebInterface(new Service);
    router.rebuild();

    listenHTTP("0.0.0.0:3000", router);
    runApplication();
}
