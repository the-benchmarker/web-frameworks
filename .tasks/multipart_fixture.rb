# frozen_string_literal: true

require 'fileutils'

source, destination, boundary = ARGV
abort 'usage: ruby multipart_fixture.rb SOURCE DESTINATION BOUNDARY' unless source && destination && boundary

payload = File.binread(source)
abort "expected a 4096-byte upload fixture, got #{payload.bytesize} bytes" unless payload.bytesize == 4096

body = +"--#{boundary}\r\n"
body << "Content-Disposition: form-data; name=\"file\"; filename=\"#{File.basename(source)}\"\r\n"
body << "Content-Type: application/octet-stream\r\n\r\n"
body = body.b
body << payload
body << "\r\n--#{boundary}--\r\n"

FileUtils.mkdir_p(File.dirname(destination))
temporary = "#{destination}.#{Process.pid}.tmp"
begin
  File.binwrite(temporary, body)
  File.rename(temporary, destination)
ensure
  File.delete(temporary) if File.exist?(temporary)
end
