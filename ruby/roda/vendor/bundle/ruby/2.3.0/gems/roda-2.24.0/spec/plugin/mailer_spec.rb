require File.expand_path("spec_helper", File.dirname(File.dirname(__FILE__)))

begin
  require 'mail'
rescue LoadError
  warn "mail not installed, skipping mail plugin test"  
else
Mail.defaults do
  delivery_method :test
end

describe "mailer plugin" do 
  def deliveries
    Mail::TestMailer.deliveries
  end

  after do
    deliveries.clear
  end

  setup_email = lambda do 
    from "f@example.com"
    to "t@example.com"
    subject 's'
  end

  it "supports sending emails via the routing tree" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        cc "c@example.com"
        bcc "b@example.com"
        response['X-Foo'] = 'Bar'
        "b"
      end
    end

    m = app.mail('/foo')
    deliveries.must_equal []
    m.from.must_equal ['f@example.com']
    m.to.must_equal ['t@example.com']
    m.cc.must_equal ['c@example.com']
    m.bcc.must_equal ['b@example.com']
    m.subject.must_equal 's'
    m.body.must_be :==, 'b'
    m.header['X-Foo'].to_s.must_equal 'Bar'

    m.deliver!
    deliveries.must_equal [m]

    deliveries.clear
    m = app.sendmail('/foo')
    deliveries.must_equal [m]
    m.from.must_equal ['f@example.com']
    m.to.must_equal ['t@example.com']
    m.cc.must_equal ['c@example.com']
    m.bcc.must_equal ['b@example.com']
    m.subject.must_equal 's'
    m.body.must_be :==, 'b'
    m.header['X-Foo'].to_s.must_equal 'Bar'
  end

  it "supports arguments to mail/sendmail methods, yielding them to the route blocks" do
    app(:mailer) do |r|
      instance_exec(&setup_email)
      r.mail "foo" do |*args|
        "foo#{args.inspect}"
      end
      r.mail :d do |*args|
        args.inspect
      end
    end

    app.mail('/foo', 1, 2).body.must_be :==, 'foo[1, 2]'
    app.sendmail('/bar', 1, 2).body.must_be :==, '["bar", 1, 2]'
  end

  it "supports no_mail! method for skipping mailing" do
    app(:mailer) do |r|
      instance_exec(&setup_email)
      r.mail "foo" do |*args|
        no_mail!
        raise
      end
    end

    app.mail('/foo', 1, 2).must_be_nil
    app.sendmail('/foo', 1, 2).must_be_nil
    deliveries.must_equal []
  end

  it "supports attachments" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        add_file __FILE__
      end
    end

    m = app.mail('foo')
    m.attachments.length.must_equal 1
    m.attachments.first.content_type.must_match(/mailer_spec\.rb/)
    m.content_type.must_match(/\Amultipart\/mixed/)
    m.parts.length.must_equal 1
    m.parts.first.body.must_be :==, File.read(__FILE__)
  end

  it "supports attachments with blocks" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        add_file __FILE__ do
          response.mail.attachments.last.content_type = 'text/foo'
        end
      end
    end

    m = app.mail('foo')
    m.attachments.length.must_equal 1
    m.attachments.first.content_type.must_equal 'text/foo'
    m.content_type.must_match(/\Amultipart\/mixed/)
    m.parts.length.must_equal 1
    m.parts.first.body.must_be :==, File.read(__FILE__)
  end

  it "supports plain-text attachments with an email body" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        add_file :filename=>'a.txt', :content=>'b'
        'c'
      end
    end

    m = app.mail('foo')
    m.parts.length.must_equal 2
    m.parts.first.content_type.must_match(/text\/plain/)
    m.parts.first.body.must_be :==, 'c'
    m.parts.last.content_type.must_match(/text\/plain/)
    m.parts.last.body.must_be :==, 'b'
    m.attachments.length.must_equal 1
    m.attachments.first.content_type.must_match(/a\.txt/)
    m.content_type.must_match(/\Amultipart\/mixed/)
  end

  it "supports regular web requests in same application" do
    app(:mailer) do |r|
      r.get "foo", :bar do |bar|
        "foo#{bar}"
      end
      r.mail "bar" do
        instance_exec(&setup_email)
        "b"
      end
    end

    body("/foo/baz", 'rack.input'=>StringIO.new).must_equal 'foobaz'
    app.mail('/bar').body.must_be :==, 'b'
  end

  it "supports multipart email using text_part/html_pat" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        text_part "t"
        html_part "h"
      end
    end

    m = app.mail('/foo')
    m.text_part.body.must_be :==, 't'
    m.html_part.body.must_be :==, 'h'
    m.content_type.must_match(/\Amultipart\/alternative/)
  end

  it "supports setting arbitrary email headers for multipart emails" do
    app(:mailer) do |r|
      r.mail do
        instance_exec(&setup_email)
        text_part "t", "X-Text"=>'T'
        html_part "h", "X-HTML"=>'H'
      end
    end

    m = app.mail('/foo')
    m.text_part.body.must_be :==, 't'
    m.text_part.header['X-Text'].to_s.must_equal 'T'
    m.html_part.body.must_be :==, 'h'
    m.html_part.header['X-HTML'].to_s.must_equal 'H'
    m.content_type.must_match(/\Amultipart\/alternative/)
  end

  it "raises error if mail object is not returned" do
    app(:mailer){}
    proc{app.mail('/')}.must_raise(Roda::RodaPlugins::Mailer::Error)
  end

  it "does not raise an error when using an explicitly empty body" do
    app(:mailer){""}
    app.mail('/')
  end

  it "supports setting the default content-type for emails when loading the plugin" do
    app(:bare) do
      plugin :mailer, :content_type=>'text/html'
      route{""}
    end
    app.mail('/').content_type.must_match(/\Atext\/html/)
  end

  it "supports loading the plugin multiple times" do
    app(:bare) do
      plugin :mailer, :content_type=>'text/html'
      plugin :mailer
      route{""}
    end
    app.mail('/').content_type.must_match(/\Atext\/html/)
  end

  it "supports manually overridding the default content-type for emails" do
    app(:bare) do
      plugin :mailer, :content_type=>'text/html'
      route do
        response['Content-Type'] = 'text/foo'
        ""
      end
    end
    app.mail('/').content_type.must_match(/\Atext\/foo/)
  end

  it "supports setting the default content type when attachments are used" do
    app(:bare) do
      plugin :mailer, :content_type=>'text/html'
      route do
        add_file 'spec/assets/css/raw.css'
        "a"
      end
    end
    m = app.mail('/')
    m.content_type.must_match(/\Amultipart\/mixed/)
    m.parts.length.must_equal 2
    m.parts.first.content_type.must_match(/\Atext\/html/)
    m.parts.first.body.must_be :==, "a"
    m.parts.last.content_type.must_match(/\Atext\/css/)
    m.parts.last.body.must_be :==, File.read('spec/assets/css/raw.css')
  end
end
end
