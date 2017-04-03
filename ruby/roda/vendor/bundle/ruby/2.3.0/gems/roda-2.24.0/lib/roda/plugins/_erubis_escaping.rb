# frozen-string-literal: true

require 'erubis'

class Roda
  module RodaPlugins
    # The _erubis_escaping plugin handles escaping of <tt><%= %></tt> inside
    # ERB templates.  It is an internal plugin that should not be loaded
    # directlyn by user code.
    module ErubisEscaping
      # Escaper which escapes by default, but does not escape instances of
      # classes marked as safe.
      class UnsafeClassEscaper
        # Default escaper if the string needs to be escaped.
        Escaper = Erubis::XmlHelper

        # Record the classes to consider safe.
        def initialize(safe_classes)
          @safe_classes = Array(safe_classes).freeze
          freeze
        end

        # If the string given is not an instance of one of the safe
        # classes, escape it, otherwise return it verbatim.  If the
        # given object is not already a string, convert it to a string first.
        def escape_xml(string)
          unless string.is_a?(String)
            string = string.to_s
          end

          if @safe_classes.any?{|c| string.is_a?(c)}
            string
          else
            Escaper.escape_xml(string)
          end
        end
      end

      # Subclass that works with specified escaper, also handling
      # postfix conditionals inside <tt><%= %></tt> tags.
      class Eruby < Erubis::EscapedEruby
        # Set escaping object to a local variable
        def convert_input(codebuf, input)
          codebuf << '_erubis_escaper = render_opts[:escaper];'
          super
        end

        # Use escaping object to escape the code, and handle postfix conditionals.
        def add_expr_escaped(src, code)
          src << " " << @bufvar << " << _erubis_escaper.escape_xml((" <<  code << "));"
        end
      end
    end

    register_plugin(:_erubis_escaping, ErubisEscaping)
  end
end
