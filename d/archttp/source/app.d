import archttp; 
import std.parallelism: totalCPUs;
 
void main(string[] args) 
{ 
    Archttp app  = new Archttp(totalCPUs); 
 
    app.get("/", (HttpRequest req, HttpResponse res) { 
        res.send(""); 
    }); 
 
    app.get("/user/{id}", (HttpRequest req, HttpResponse res) { 
        res.send(req.params["id"]); 
    }); 
 
    app.post("/user", (HttpRequest req, HttpResponse res) { 
        res.send(""); 
    }); 
 
    app.listen(3000); 
} 
