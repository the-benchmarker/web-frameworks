import { useBrahma, startServer } from "brahma-firelight";

useBrahma((req) => {
    const { path, body } = req;

    // GET --> /
    if (path === "/") {
        return {
            headers: { "Content-Type": "text/plain" },
            status: 200,
            body: "",
        };
    }

    // POST --> /user
    const bodyLen = body ? (typeof body.length === "number" ? body.length : String(body).length) : 0;
    if (path === "/user" && bodyLen > 0) {
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
