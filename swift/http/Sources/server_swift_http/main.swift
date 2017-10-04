import HTTP
import Foundation



let server = HTTPServer()

class WebApp: HTTPRequestHandling {
    func handle(request: HTTPRequest, response: HTTPResponseWriter ) -> HTTPBodyProcessing {

        switch request.method {
        case HTTPMethod.get:
            // GET '/' return status code 200 with empty body
            if request.target == "/" {
                response.writeHeader(status: .ok, headers: [.contentLength: "0"])
                return .discardBody
            }
            // GET '/user/:id' return status code 200 with the id
            else {
                let idIndex = request.target.index(request.target.startIndex, offsetBy: 6)
                let user = request.target[..<idIndex]
                if user == "/user/" {
                    let result = request.target[idIndex...]
                    let data = Data(result.utf8)
                    response.writeHeader(status: .ok, headers: [.contentLength: "\(data.count)"])
                    response.writeBody(data)
                    return .discardBody
                }
            }
        case HTTPMethod.post:
            // POST '/user' return status code 200 with empty body
            if request.target == "/user" {
                response.writeHeader(status: .ok, headers: [.contentLength: "0"])
                return .discardBody
            }
        default:
            response.writeHeader(status: .badRequest)
            return .discardBody
        }
        return .discardBody
    }
	
}

try! server.start(port: 3000, handler: WebApp().handle)

CFRunLoopRun()

