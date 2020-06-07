# frozen_string_literal: true

module Web
  class Application < Hanami::Application
    configure do
      root __dir__

      load_paths << [
        "controllers"
      ]

      routes "config/routes"

      # SECURITY

      security.x_frame_options("DENY")
      security.x_content_type_options("nosniff")
      security.x_xss_protection("1; mode=block")
      security.content_security_policy(%(
        form-action 'self';
        frame-ancestors 'self';
        base-uri 'self';
        default-src 'none';
        script-src 'self';
        connect-src 'self';
        img-src 'self' https: data:;
        style-src 'self' 'unsafe-inline' https:;
        font-src 'self';
        object-src 'none';
        plugin-types application/pdf;
        child-src 'self';
        frame-src 'self';
        media-src 'self'
      ))
    end
  end
end
