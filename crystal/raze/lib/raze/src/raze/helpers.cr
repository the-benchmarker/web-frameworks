module Raze::Helpers
  def self.send_file(ctx, path : String, mime_type : String? = nil)
    file_path = File.expand_path(path, Dir.current)
    mime_type ||= Raze::Utils.mime_type(file_path)
    ctx.response.content_type = mime_type

    File.open(file_path) { |file| self.handle_send_file(ctx, file, file_path) }
    return
  end

  def self.handle_send_file(ctx, file, file_path)
    if ctx.request.method == "GET" && ctx.request.headers.has_key?("Range")
      return self.multipart(file, ctx)
    end

    file_size = File.size(file_path)
    file_ext = File.extname(file_path)

    if self.should_compress?(ctx.request.headers, file_size, file_ext, "gzip")
      self.compress_file(ctx, "gzip", file)
    elsif self.should_compress?(ctx.request.headers, file_size, file_ext, "deflate")
      self.compress_file(ctx, "deflate", file)
    else
      ctx.response.content_length = file_size
      IO.copy(file, ctx.response)
    end
  end

  # Determines if the asset should be compressed
  def self.should_compress?(headers, file_size, file_ext, compression_type)
    minsize = 860 # http://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits

    headers.includes_word?("Accept-Encoding", compression_type) &&
      Raze.config.compress &&
      file_size > minsize &&
      Raze::Utils.zip_types(file_ext)
  end

  def self.compress_file(ctx, compression_type, file)
    ctx.response.headers["Content-Encoding"] = compression_type

    if compression_type == "gzip"
      Gzip::Writer.open(ctx.response) do |deflate|
        IO.copy(file, deflate)
      end
    elsif compression_type == "deflate"
      Flate::Writer.new(ctx.response) do |deflate|
        IO.copy(file, deflate)
      end
    end
  end

  def self.multipart(file, ctx)
    # See http://httpwg.org/specs/rfc7233.html
    fileb = file.size

    range = ctx.request.headers["Range"]
    match = range.match(/bytes=(\d{1,})-(\d{0,})/)

    startb = 0
    endb = 0

    if match
      if match.size >= 2
        startb = match[1].to_i { 0 }
      end

      if match.size >= 3
        endb = match[2].to_i { 0 }
      end
    end

    if endb == 0
      endb = fileb
    end

    if startb < endb && endb <= fileb
      ctx.response.status_code = 206
      ctx.response.content_length = endb - startb
      ctx.response.headers["Accept-Ranges"] = "bytes"
      ctx.response.headers["Content-Range"] = "bytes #{startb}-#{endb - 1}/#{fileb}" # MUST

      if startb > 1024
        skipped = 0
        # file.skip only accepts values less or equal to 1024 (buffer size, undocumented)
        until skipped + 1024 > startb
          file.skip(1024)
          skipped += 1024
        end
        if skipped - startb > 0
          file.skip(skipped - startb)
        end
      else
        file.skip(startb)
      end

      IO.copy(file, ctx.response, endb - startb)
    else
      ctx.response.content_length = fileb
      ctx.response.status_code = 200 # Range not satisfable, see 4.4 Note
      IO.copy(file, ctx.response)
    end
  end
end
