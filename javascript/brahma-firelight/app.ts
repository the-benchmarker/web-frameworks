import { useBrahma, startServer } from "brahma-firelight";

useBrahma((req) => {
    const { path } = req;

    // GET --> /
    if (path === "/") {
        return {
            headers: { "Content-Type": "text/plain" },
            status: 200,
            body: "",
        };
    }

    // POST --> /user
    if (path === "/user") {
        return {
            headers: { "Content-Type": "text/plain" },
            status: 200,
            body: "",
        };
    }

    // GET --> /user/:id
    let id = null;
    const m = path.match(/^\/user\/([^/]+)$/);
    if (m) id = decodeURIComponent(m[1]);

    if (id !== null) {
        return {
            headers: { "Content-Type": "text/plain" },
            status: 200,
            body: `${id}`,
        };
    }
    // Fallback to 404 - Not Found
    return {
        status: 404,
        body: "Route not found",
    };
});

const port = 3000;
const host = "0.0.0.0";
startServer(host, +port);
