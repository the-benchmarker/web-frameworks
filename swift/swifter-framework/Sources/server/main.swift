import Swifter
import Dispatch

let server = HttpServer()

server.GET["/"] = { _ in
    return HttpResponse.ok(.text(""))
}

server.GET["/user/:id"] = { request in
    let userId = request.params[":id"] ?? ""
    return HttpResponse.ok(.text("\(userId)"))
}

server.POST["/user"] = { _ in
    return HttpResponse.ok(.text(""))
}

let semaphore = DispatchSemaphore(value: 0)
do {
  try server.start(3000, forceIPv4: true)
  print("Server has started ( port = \(try server.port()) ). Try to connect now...")
  semaphore.wait()
} catch {
  print("Server start error: \(error)")
  semaphore.signal()
}
