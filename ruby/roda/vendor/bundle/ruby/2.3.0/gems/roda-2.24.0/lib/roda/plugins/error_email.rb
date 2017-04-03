# frozen-string-literal: true

require 'net/smtp'

class Roda
  module RodaPlugins
    # The error_email plugin adds an +error_email+ instance method that
    # send an email related to the exception.  This is most useful if you are
    # also using the error_handler plugin:
    #
    #   plugin :error_email, :to=>'to@example.com', :from=>'from@example.com'
    #   plugin :error_handler do |e|
    #     error_email(e)
    #     'Internal Server Error'
    #   end
    #
    # Options:
    #
    # :from :: The From address to use in the email (required)
    # :headers :: A hash of additional headers to use in the email (default: empty hash)
    # :host :: The SMTP server to use to send the email (default: localhost)
    # :prefix :: A prefix to use in the email's subject line (default: no prefix)
    # :to :: The To address to use in the email (required)
    #
    # The subject of the error email shows the exception class and message.
    # The body of the error email shows the backtrace of the error and the
    # request environment, as well the request params and session variables (if any).
    # You can also call error_email with a plain string instead of an exception,
    # in which case the string is used as the subject, and no backtrace is included.
    #
    # Note that emailing on every error as shown above is only appropriate
    # for low traffic web applications.  For high traffic web applications,
    # use an error reporting service instead of this plugin.
    module ErrorEmail
      OPTS = {}.freeze
      DEFAULTS = {
        :headers=>{},
        :host=>'localhost',
        # :nocov:
        :emailer=>lambda{|h| Net::SMTP.start(h[:host]){|s| s.send_message(h[:message], h[:from], h[:to])}},
        # :nocov:
        :default_headers=>lambda do |h, e|
          subject = if e.respond_to?(:message)
            "#{e.class}: #{e.message}"
          else
            e.to_s
          end
          {'From'=>h[:from], 'To'=>h[:to], 'Subject'=>"#{h[:prefix]}#{subject}"}
        end,
        :body=>lambda do |s, e|
          format = lambda{|h| h.map{|k, v| "#{k.inspect} => #{v.inspect}"}.sort.join("\n")}

          message = String.new
          message << <<END
Path: #{s.request.path}

END
          if e.respond_to?(:backtrace)
            message << <<END
Backtrace:

#{e.backtrace.join("\n")}
END
          end

          message << <<END
ENV:

#{format[s.env]}
END

          unless s.request.params.empty?
            message << <<END

Params:

#{format[s.request.params]}
END
          end

          if s.env['rack.session']
            message << <<END

Session:

#{format[s.session]}
END
          end

          message
        end
      }

      # Set default opts for plugin.  See ErrorEmail module RDoc for options.
      def self.configure(app, opts=OPTS)
        email_opts = app.opts[:error_email] ||= DEFAULTS
        email_opts = email_opts.merge(opts)
        email_opts[:headers] = email_opts[:headers].dup
        unless email_opts[:to] && email_opts[:from]
          raise RodaError, "must provide :to and :from options to error_email plugin"
        end
        app.opts[:error_email] = email_opts
        app.opts[:error_email][:headers].freeze
        app.opts[:error_email].freeze
      end

      module InstanceMethods
        # Send an email for the given error.  +exception+ is usually an exception
        # instance, but it can be a plain string which is used as the subject for
        # the email.
        def error_email(exception)
          email_opts = self.class.opts[:error_email].dup
          email_opts[:message] = error_email_content(exception)
          email_opts[:emailer].call(email_opts)
        end

        # The content of the email to send, include the headers and the body.
        # Takes the same argument as #error_email.
        def error_email_content(exception)
          email_opts = self.class.opts[:error_email]
          headers = email_opts[:default_headers].call(email_opts, exception)
          headers = Hash[headers].merge!(email_opts[:headers])
          headers = headers.map{|k,v| "#{k}: #{v.gsub(/\r?\n/m, "\r\n ")}"}.sort.join("\r\n")
          body = email_opts[:body].call(self, exception)
          "#{headers}\r\n\r\n#{body}"
        end
      end
    end

    register_plugin(:error_email, ErrorEmail)
  end
end
