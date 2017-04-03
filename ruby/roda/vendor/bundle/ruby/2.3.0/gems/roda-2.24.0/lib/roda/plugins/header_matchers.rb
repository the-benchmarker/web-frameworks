# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The header_matchers plugin adds hash matchers for matching on less-common
    # HTTP headers.
    #
    #   plugin :header_matchers
    #
    # It adds a +:header+ matcher for matching on arbitrary headers, which matches
    # if the header is present:
    #
    #   r.on :header=>'HTTP-X-App-Token' do |header_value|
    #     # Looks for env['HTTP_X_APP_TOKEN']
    #   end
    #
    # For backwards compatibility, the header value is not automatically prefixed
    # with HTTP_.  You can set the +:header_matcher_prefix+ option for the application,
    # which will automatically prefix the header with HTTP_:
    #
    #   r.on :header=>'X-App-Token' do |header_value|
    #     # Looks for env['HTTP_X_APP_TOKEN'] 
    #   end
    #
    # It adds a +:host+ matcher for matching by the host of the request:
    #
    #   r.on :host=>'foo.example.com' do
    #   end
    #   r.on :host=>/\A\w+.example.com\z/ do
    #   end
    #
    # By default the +:host+ matcher does not yield matchers, but if you use a regexp
    # and set the +:host_matcher_captures+ option for the application, it will
    # yield regexp captures:
    # 
    #   r.on :host=>/\A(\w+).example.com\z/ do |subdomain|
    #   end
    #
    # It adds a +:user_agent+ matcher for matching on a user agent patterns, which
    # yields the regexp captures to the block:
    #
    #   r.on :user_agent=>/Chrome\/([.\d]+)/ do |chrome_version|
    #   end
    #
    # It adds an +:accept+ matcher for matching based on the Accept header:
    #
    #   r.on :accept=>'text/csv' do
    #   end
    #
    # Note that the accept matcher is very simple and cannot handle wildcards,
    # priorities, or anything but a simple comma separated list of mime types.
    module HeaderMatchers
      module RequestMethods
        private

        # Match if the given mimetype is one of the accepted mimetypes.
        def match_accept(mimetype)
          if @env["HTTP_ACCEPT"].to_s.split(',').any?{|s| s.strip == mimetype}
            response["Content-Type"] = mimetype
          end
        end

        # Match if the given uppercase key is present inside the environment.
        def match_header(key)
          key = key.upcase.tr("-","_")

          if roda_class.opts[:header_matcher_prefix]
            key = "HTTP_#{key}"
          end

          if v = @env[key]
            @captures << v
          end
        end

        # Match if the host of the request is the same as the hostname.  +hostname+
        # can be a regexp or a string.
        def match_host(hostname)
          if hostname.is_a?(Regexp) && roda_class.opts[:host_matcher_captures]
            if match = hostname.match(host)
              @captures.concat(match.captures)
            end
          else
            hostname === host
          end
        end

        # Match the submitted user agent to the given pattern, capturing any
        # regexp match groups.
        def match_user_agent(pattern)
          if (user_agent = @env["HTTP_USER_AGENT"]) && user_agent.to_s =~ pattern
            @captures.concat($~[1..-1])
          end
        end
      end
    end

    register_plugin(:header_matchers, HeaderMatchers)
  end
end
