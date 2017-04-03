# frozen-string-literal: true

require 'stringio'
require 'mail'

class Roda
  module RodaPlugins
    # The mailer plugin allows your Roda application to send emails easily.
    #
    #   class App < Roda
    #     plugin :render
    #     plugin :mailer
    #
    #     route do |r|
    #       r.on "albums" do
    #         r.mail "added" do |album|
    #           @album = album
    #           from 'from@example.com'
    #           to 'to@example.com'
    #           cc 'cc@example.com'
    #           bcc 'bcc@example.com'
    #           subject 'Album Added'
    #           add_file "path/to/album_added_img.jpg"
    #           render(:albums_added_email) # body
    #         end
    #       end
    #     end
    #   end
    #
    # The default method for sending a mail is +sendmail+:
    #
    #   App.sendmail("/albums/added", Album[1])
    #
    # If you want to return the <tt>Mail::Message</tt> instance for further modification,
    # you can just use the +mail+ method:
    #
    #   mail = App.mail("/albums/added", Album[1])
    #   mail.from 'from2@example.com'
    #   mail.deliver
    #
    # The mailer plugin uses the mail gem, so if you want to configure how
    # email is sent, you can use <tt>Mail.defaults</tt> (see the mail gem documentation for
    # more details):
    #
    #   Mail.defaults do
    #     delivery_method :smtp, :address=>'smtp.example.com', :port=>587
    #   end
    #
    # You can support multipart emails using +text_part+ and +html_part+:
    #
    #   r.mail "added" do |album_added|
    #     from 'from@example.com'
    #     to 'to@example.com'
    #     subject 'Album Added'
    #     text_part render('album_added.txt')  # views/album_added.txt.erb
    #     html_part render('album_added.html') # views/album_added.html.erb
    #   end
    #
    # In addition to allowing you to use Roda's render plugin for rendering
    # email bodies, you can use all of Roda's usual routing tree features
    # to DRY up your code:
    #
    #   r.on "albums", :d do |album_id|
    #     @album = Album[album_id.to_i]
    #     from 'from@example.com'
    #     to 'to@example.com'
    #
    #     r.mail "added" do
    #       subject 'Album Added'
    #       render(:albums_added_email)
    #     end
    #
    #     r.mail "deleted" do
    #       subject 'Album Deleted'
    #       render(:albums_deleted_email)
    #     end
    #   end
    #
    # When sending a mail via +mail+ or +sendmail+, a RodaError will be raised
    # if the mail object does not have a body.  This is similar to the 404
    # status that Roda uses by default for web requests that don't have
    # a body. If you want to specifically send an email with an empty body, you
    # can use the explicit empty string:
    #
    #   r.mail do
    #     from 'from@example.com'
    #     to 'to@example.com'
    #     subject 'No Body Here'
    #     ""
    #   end
    #
    # If while preparing the email you figure out you don't want to send an
    # email, call +no_mail!+:
    #
    #  r.mail 'welcome', :d do |id| 
    #    no_mail! unless user = User[id]
    #    # ...
    #  end
    #
    # By default, the mailer uses text/plain as the Content-Type for emails.
    # You can override the default by specifying a :content_type option when
    # loading the plugin:
    #
    #   plugin :mailer, :content_type=>'text/html'
    #
    # The mailer plugin does support being used inside a Roda application
    # that is handling web requests, where the routing block for mails and
    # web requests is shared.  However, it's recommended that you create a
    # separate Roda application for emails. This can be a subclass of your main
    # Roda application if you want your helper methods to automatically be
    # available in your email views.
    module Mailer
      REQUEST_METHOD = "REQUEST_METHOD".freeze
      PATH_INFO = "PATH_INFO".freeze
      SCRIPT_NAME = 'SCRIPT_NAME'.freeze
      EMPTY_STRING = ''.freeze
      RACK_INPUT = 'rack.input'.freeze
      RODA_MAIL = 'roda.mail'.freeze
      RODA_MAIL_ARGS = 'roda.mail_args'.freeze
      MAIL = "MAIL".freeze
      CONTENT_TYPE = 'Content-Type'.freeze
      TEXT_PLAIN = "text/plain".freeze
      OPTS = {}.freeze

      # Error raised when the using the mail class method, but the routing
      # tree doesn't return the mail object. 
      class Error < ::Roda::RodaError; end

      # Set the options for the mailer.  Options:
      # :content_type :: The default content type for emails (default: text/plain)
      def self.configure(app, opts=OPTS)
        app.opts[:mailer] = (app.opts[:mailer]||{}).merge(opts).freeze
      end

      module ClassMethods
        # Return a Mail::Message instance for the email for the given request path
        # and arguments.  You can further manipulate the returned mail object before
        # calling +deliver+ to send the mail.
        def mail(path, *args)
          mail = ::Mail.new
          catch(:no_mail) do
            unless mail.equal?(new(PATH_INFO=>path, SCRIPT_NAME=>EMPTY_STRING, REQUEST_METHOD=>MAIL, RACK_INPUT=>StringIO.new, RODA_MAIL=>mail, RODA_MAIL_ARGS=>args).call(&route_block))
              raise Error, "route did not return mail instance for #{path.inspect}, #{args.inspect}"
            end
            mail
          end
        end

        # Calls +mail+ and immediately sends the resulting mail.
        def sendmail(*args)
          if m = mail(*args)
            m.deliver
          end
        end
      end

      module RequestMethods
        # Similar to routing tree methods such as +get+ and +post+, this matches
        # only if the request method is MAIL (only set when using the Roda class
        # +mail+ or +sendmail+ methods) and the rest of the arguments match
        # the request.  This yields any of the captures to the block, as well as
        # any arguments passed to the +mail+ or +sendmail+ Roda class methods.
        def mail(*args)
          if @env[REQUEST_METHOD] == MAIL
            if_match(args) do |*vs|
              yield(*(vs + @env[RODA_MAIL_ARGS]))
            end
          end
        end
      end

      module ResponseMethods
        # The mail object related to the current request.
        attr_accessor :mail

        # If the related request was an email request, add any response headers
        # to the email, as well as adding the response body to the email.
        # Return the email unless no body was set for it, which would indicate
        # that the routing tree did not handle the request.
        def finish
          if m = mail
            header_content_type = @headers.delete(CONTENT_TYPE)
            m.headers(@headers)
            m.body(@body.join) unless @body.empty?
            mail_attachments.each do |a, block|
              m.add_file(*a)
              block.call if block
            end

            if content_type = header_content_type || roda_class.opts[:mailer][:content_type]
              if mail.multipart?
                if mail.content_type =~ /multipart\/mixed/ &&
                   mail.parts.length >= 2 &&
                   (part = mail.parts.find{|p| !p.attachment && p.content_type == TEXT_PLAIN})
                  part.content_type = content_type
                end
              else
                mail.content_type = content_type
              end
            end

            unless m.body.to_s.empty? && m.parts.empty? && @body.empty?
              m
            end
          else
            super
          end
        end

        # The attachments related to the current mail.
        def mail_attachments
          @mail_attachments ||= []
        end
      end

      module InstanceMethods
        # Add delegates for common email methods.
        [:from, :to, :cc, :bcc, :subject].each do |meth|
          define_method(meth) do |*args|
            env[RODA_MAIL].send(meth, *args)
            nil
          end
        end
        [:text_part, :html_part].each do |meth|
          define_method(meth) do |*args|
            _mail_part(meth, *args)
          end
        end

        # If this is an email request, set the mail object in the response, as well
        # as the default content_type for the email.
        def initialize(env)
          super
          if mail = env[RODA_MAIL]
            res = @_response
            res.mail = mail
            res.headers.delete(CONTENT_TYPE)
          end
        end

        # Delay adding a file to the message until after the message body has been set.
        # If a block is given, the block is called after the file has been added, and you
        # can access the attachment via <tt>response.mail.attachments.last</tt>.
        def add_file(*a, &block)
          response.mail_attachments << [a, block]
          nil
        end

        # Signal that no mail should be sent for this request.
        def no_mail!
          throw :no_mail
        end

        private

        # Set the text_part or html_part (depending on the method) in the related email,
        # using the given body and optional headers.
        def _mail_part(meth, body, headers=nil)
          env[RODA_MAIL].send(meth) do
            body(body)
            headers(headers) if headers
          end
          nil
        end
      end
    end

    register_plugin(:mailer, Mailer)
  end
end
