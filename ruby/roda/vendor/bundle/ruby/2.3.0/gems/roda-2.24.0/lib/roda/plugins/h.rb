# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The h plugin adds an +h+ instance method that will HTML
    # escape the input and return it.
    #
    # The following example will return "&lt;foo&gt;" as the body.
    #
    #   plugin :h
    #
    #   route do |r|
    #     h('<foo>')
    #   end
    module H
      begin
        require 'cgi/escape'
        unless CGI.respond_to?(:escapeHTML) # work around for JRuby 9.1
          CGI = Object.new
          CGI.extend(::CGI::Util)
        end

        module InstanceMethods
          # HTML escape the input and return the escaped version.
          def h(string)
            CGI.escapeHTML(string.to_s)
          end
        end
      rescue LoadError
        # A Hash of entities and their escaped equivalents,
        # to be escaped by h().
        ESCAPE_HTML = {
          "&" => "&amp;".freeze,
          "<" => "&lt;".freeze,
          ">" => "&gt;".freeze,
          "'" => "&#39;".freeze,
          '"' => "&quot;".freeze,
        }.freeze

        # A Regexp of HTML entities to match for escaping.
        ESCAPE_HTML_PATTERN = Regexp.union(*ESCAPE_HTML.keys)

        module InstanceMethods
          def h(string)
            string.to_s.gsub(ESCAPE_HTML_PATTERN){|c| ESCAPE_HTML[c] }
          end
        end
      end
    end

    register_plugin(:h, H)
  end
end
