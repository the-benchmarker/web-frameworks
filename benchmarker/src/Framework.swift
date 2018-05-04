import Vapor

struct Framework: Codable, CustomStringConvertible {
    static func readAll() throws -> [Framework] {
        guard let data = FileManager.default.contents(atPath: "frameworks.json") else {
            throw "No `frameworks.json` file."
        }
        return try JSONDecoder().decode([Framework].self, from: data)
    }

    var name: String
    var language: String
    var port: Int
    var description: String {
        return "\(name) (\(language))"
    }
}
