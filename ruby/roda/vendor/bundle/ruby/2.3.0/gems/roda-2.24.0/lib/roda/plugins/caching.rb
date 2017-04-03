# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The caching plugin adds methods related to HTTP caching.
    #
    # For proper caching, you should use either the +last_modified+ or
    # +etag+ request methods.  
    #
    #   r.get 'albums', :d do |album_id|
    #     @album = Album[album_id]
    #     r.last_modified @album.updated_at
    #     view('album')
    #   end
    #
    #   # or
    #
    #   r.get 'albums', :d do |album_id|
    #     @album = Album[album_id]
    #     r.etag @album.sha1
    #     view('album')
    #   end
    #
    # Both +last_modified+ or +etag+ will immediately halt processing
    # if there have been no modifications since the last time the
    # client requested the resource, assuming the client uses the
    # appropriate HTTP 1.1 request headers.
    #
    # This plugin also includes the +cache_control+ and +expires+
    # response methods.  The +cache_control+ method sets the
    # Cache-Control header using the given hash:
    #
    #   response.cache_control :public=>true, :max_age=>60
    #   # Cache-Control: public, max-age=60
    # 
    # The +expires+ method is similar, but in addition
    # to setting the HTTP 1.1 Cache-Control header, it also sets
    # the HTTP 1.0 Expires header:
    #
    #   response.expires 60, :public=>true
    #   # Cache-Control: public, max-age=60
    #   # Expires: Mon, 29 Sep 2014 21:25:47 GMT
    #
    # The implementation was originally taken from Sinatra,
    # which is also released under the MIT License:
    #
    # Copyright (c) 2007, 2008, 2009 Blake Mizerany
    # Copyright (c) 2010, 2011, 2012, 2013, 2014 Konstantin Haase
    # 
    # Permission is hereby granted, free of charge, to any person
    # obtaining a copy of this software and associated documentation
    # files (the "Software"), to deal in the Software without
    # restriction, including without limitation the rights to use,
    # copy, modify, merge, publish, distribute, sublicense, and/or sell
    # copies of the Software, and to permit persons to whom the
    # Software is furnished to do so, subject to the following
    # conditions:
    # 
    # The above copyright notice and this permission notice shall be
    # included in all copies or substantial portions of the Software.
    # 
    # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    # EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
    # OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    # NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    # HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    # WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    # FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    # OTHER DEALINGS IN THE SOFTWARE.
    module Caching
      OPTS = {}.freeze

      module RequestMethods
        LAST_MODIFIED = 'Last-Modified'.freeze
        HTTP_IF_NONE_MATCH = 'HTTP_IF_NONE_MATCH'.freeze
        HTTP_IF_MATCH = 'HTTP_IF_MATCH'.freeze
        HTTP_IF_MODIFIED_SINCE = 'HTTP_IF_MODIFIED_SINCE'.freeze
        HTTP_IF_UNMODIFIED_SINCE = 'HTTP_IF_UNMODIFIED_SINCE'.freeze
        ETAG = 'ETag'.freeze
        STAR = '*'.freeze

        # Set the last modified time of the resource using the Last-Modified header.
        # The +time+ argument should be a Time instance.
        #
        # If the current request includes an If-Modified-Since header that is
        # equal or later than the time specified, immediately returns a response
        # with a 304 status.
        #
        # If the current request includes an If-Unmodified-Since header that is
        # before than the time specified, immediately returns a response
        # with a 412 status.
        def last_modified(time)
          return unless time
          res = response
          e = env
          res[LAST_MODIFIED] = time.httpdate
          return if e[HTTP_IF_NONE_MATCH]
          status = res.status

          if (!status || status == 200) && (ims = time_from_header(e[HTTP_IF_MODIFIED_SINCE])) && ims >= time.to_i
            res.status = 304
            halt
          end

          if (!status || (status >= 200 && status < 300) || status == 412) && (ius = time_from_header(e[HTTP_IF_UNMODIFIED_SINCE])) && ius < time.to_i
            res.status = 412
            halt
          end
        end

        # Set the response entity tag using the ETag header.
        #
        # The +value+ argument is an identifier that uniquely
        # identifies the current version of the resource.
        # Options:
        # :weak :: Use a weak cache validator (a strong cache validator is the default)
        # :new_resource :: Whether this etag should match an etag of * (true for POST, false otherwise)
        #
        # When the current request includes an If-None-Match header with a
        # matching etag, immediately returns a response with a 304 or 412 status,
        # depending on the request method.
        #
        # When the current request includes an If-Match header with a
        # etag that doesn't match, immediately returns a response with a 412 status.
        def etag(value, opts=OPTS)
          # Before touching this code, please double check RFC 2616 14.24 and 14.26.
          weak = opts[:weak]
          new_resource = opts.fetch(:new_resource){post?}

          res = response
          e = env
          res[ETAG] = etag = "#{'W/' if weak}\"#{value}\""
          status = res.status

          if (!status || (status >= 200 && status < 300) || status == 304)
            if etag_matches?(e[HTTP_IF_NONE_MATCH], etag, new_resource)
              res.status = (request_method =~ /\AGET|HEAD|OPTIONS|TRACE\z/i ? 304 : 412)
              halt
            end

            if ifm = e[HTTP_IF_MATCH]
              unless etag_matches?(ifm, etag, new_resource)
                res.status = 412
                halt
              end
            end
          end
        end

        private

        # Helper method checking if a ETag value list includes the current ETag.
        def etag_matches?(list, etag, new_resource)
          return unless list
          return !new_resource if list == STAR
          list.to_s.split(/\s*,\s*/).include?(etag)
        end

        # Helper method parsing a time value from an HTTP header, returning the
        # time as an integer.
        def time_from_header(t)
          Time.httpdate(t).to_i if t
        rescue ArgumentError
        end
      end

      module ResponseMethods
        UNDERSCORE = '_'.freeze
        DASH = '-'.freeze
        COMMA = ', '.freeze
        CACHE_CONTROL = 'Cache-Control'.freeze
        EXPIRES = 'Expires'.freeze
        CONTENT_TYPE = 'Content-Type'.freeze
        CONTENT_LENGTH = 'Content-Length'.freeze

        # Specify response freshness policy for using the Cache-Control header.
        # Options can can any non-value directives (:public, :private, :no_cache,
        # :no_store, :must_revalidate, :proxy_revalidate), with true as the value.
        # Options can also contain value directives (:max_age, :s_maxage).
        #
        #   response.cache_control :public=>true, :max_age => 60
        #   # => Cache-Control: public, max-age=60
        #
        # See RFC 2616 / 14.9 for more on standard cache control directives:
        # http://tools.ietf.org/html/rfc2616#section-14.9.1
        def cache_control(opts)
          values = []
          opts.each do |k, v|
            next unless v
            k = k.to_s.tr(UNDERSCORE, DASH)
            values << (v == true ? k : "#{k}=#{v}")
          end

          self[CACHE_CONTROL] = values.join(COMMA) unless values.empty?
        end

        # Set Cache-Control header with the max_age given.  max_age should
        # be an integer number of seconds that the current request should be
        # cached for.  Also sets the Expires header, useful if you have
        # HTTP 1.0 clients (Cache-Control is an HTTP 1.1 header).
        def expires(max_age, opts=OPTS)
          cache_control(Hash[opts].merge!(:max_age=>max_age))
          self[EXPIRES] = (Time.now + max_age).httpdate
        end

        # Remove Content-Type and Content-Length for 304 responses.
        def finish
          a = super
          if a[0] == 304
            h = a[1]
            h.delete(CONTENT_TYPE)
            h.delete(CONTENT_LENGTH)
          end
          a
        end
      end
    end

    register_plugin(:caching, Caching)
  end
end
