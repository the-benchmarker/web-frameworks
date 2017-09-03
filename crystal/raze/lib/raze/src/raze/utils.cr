module Raze::Utils
  def self.zip_types(file_ext) # https://github.com/h5bp/server-configs-nginx/blob/master/nginx.conf
    [".htm", ".html", ".txt", ".css", ".js", ".svg", ".json", ".xml", ".otf", ".ttf", ".woff", ".woff2"].includes? file_ext
  end

  def self.mime_type(path)
    case File.extname(path)
    when ".css"          then "text/css"
    when ".js"           then "application/javascript"
    when ".png"          then "image/png"
    when ".jpg", ".jpeg" then "image/jpeg"
    when ".svg"          then "image/svg+xml"
    when ".gif"          then "image/gif"
    when ".ico"          then "image/x-icon"
    when ".htm", ".html" then "text/html"
    when ".txt"          then "text/plain"
    when ".xml"          then "application/xml"
    when ".json"         then "application/json"
    when ".otf", ".ttf"  then "application/font-sfnt"
    when ".woff"         then "application/font-woff"
    when ".woff2"        then "font/woff2"
    else                      "application/octet-stream"
    end
  end

  def self.parse_params(params : IO?)
    if params
      HTTP::Params.parse(params.gets_to_end)
    else
      HTTP::Params.parse("")
    end
  end

  def self.parse_params(params : String?)
    HTTP::Params.parse(params || "")
  end
end
