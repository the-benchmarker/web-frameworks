{% if !flag?(:without_zlib) %}
  require "zlib"
{% end %}

class Raze::StaticFileHandler < HTTP::StaticFileHandler
  INSTANCE = new(Raze.config.static_dir)

  def public_dir=(dir)
    @public_dir = File.expand_path dir
  end

  def call(ctx)
    return call_next(ctx) if ctx.request.path.not_nil! == "/"

    unless ctx.request.method == "GET" || ctx.request.method == "HEAD"
      if @fallthrough
        call_next(ctx)
      else
        ctx.response.status_code = 405
        ctx.response.headers.add("Allow", "GET, HEAD")
      end
      return
    end

    request_path = URI.unescape(ctx.request.path.not_nil!).rstrip "/"

    if Raze.config.dynamic_static_paths.size > 0
      if Raze.config.dynamic_static_paths.any? { |path| request_path.starts_with? path }
        resource_path = String.build do |str|
          str << @public_dir
          str << request_path
        end

        return process_request(ctx, resource_path)
      end
    end

    file_or_dir = nil
    if Raze.config.static_indexing
      file_or_dir = Raze.static_file_indexer.static_files[request_path]?

      return call_next(ctx) unless file_or_dir
    end
    resource_path = String.build do |str|
      str << @public_dir
      str << request_path
    end

    if file_or_dir
      process_request(ctx, file_or_dir, request_path, resource_path)
    else
      process_request(ctx, resource_path)
    end
  end

  private def process_request(ctx, file_type, request_path, resource_path)
    if file_type == "dir"
      ctx.response.content_type = "text/html"
      directory_listing(ctx.response, request_path, resource_path)
    elsif file_type == "file"
      return if etag(ctx, resource_path)
      Raze::Helpers.send_file(ctx, resource_path)
    else
      call_next(ctx)
    end
  end

  # dynamic-static directory
  private def process_request(ctx, resource_path)
    return if etag(ctx, resource_path)
    Raze::Helpers.send_file(ctx, resource_path)
  rescue
    call_next(ctx)
  end

  private def etag(ctx, resource_path)
    etag = %{W/"#{File.lstat(resource_path).mtime.epoch.to_s}"}

    headers = ctx.request.headers
    headers["ETag"] = etag
    return false if !headers["If-None-Match"]? || headers["If-None-Match"] != etag

    ctx.response.headers.delete "Content-Type"
    ctx.response.content_length = 0
    ctx.response.status_code = 304 # not modified
    return true
  end
end
