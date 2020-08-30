require "amber"

require "../src/controllers/application_controller"
require "../src/controllers/**"

# About Application.cr File
#
# This is Amber application main entry point. This file is responsible for loading
# initializers, classes, and all application related code in order to have
# Amber::Server boot up.
#
# > We recommend to not modify the order of the require since the order will
# affect the behavior of the application.
#
# With `Amber::Server.configure` block you can redefine the Server configuration
# settings and use ENVIRONMENT variables and/or values evaluated at runtime.
#
# > Important! Yaml configurations are first class citizen and are loaded first before
# this file, we recommend to use yaml configurations before changing any settings here.
# Any uncommented setting here will override the YAML with the value set here.

Amber::Server.configure do |settings|
  # Use your environment variables settings here.
  #
  # Name: A name that identifies this application. This is not internally
  # used by the framework.
  #
  settings.name = "Amber"
  #
  #
  # Colorize Logging: specifies whether or not to use ANSI color codes
  # when logging information, display the time and/or to display the severity level.
  # Defaults to true.
  #
  # Log Level defines the verbosity of the Amber logging. This option defaults to
  # debug for all environments. The available log levels are: debug, info, warn,
  # error, fatal, and unknown.
  #
  # settings.logging.colorize = true
  # settings.logging.severity = "debug"
  # settings.logging.filter = %w(password confirm_password)
  # settings.logging.skip = %w()
  # settings.logging.context = %w(request headers cookies session params)
  #
  #
  # Secret Key Base: is used for specifying a key which allows sessions
  # for the application to be verified against a known secure key to
  # prevent tampering. Applications get Amber.secret_key
  # initialized to a random key present in `ENV["AMBER_SECRET_KEY"]` or
  # `.amber_secret_key` in this order.
  #
  # settings.secret_key_base= sa7tQoBvS1FtX_48zZBReUZ9XO26Agd3jpUwEvLr9HI
  #
  #
  # Host: is the application server host address or ip address. Useful for when
  # deploying Amber to a PAAS and likely the assigned server IP is either
  # known or unknown. Defaults to an environment variable HOST
  #
  settings.host = "0.0.0.0"
  #
  #
  # Port Reuse: Amber supports clustering mode which allows to spin
  # multiple app instances per core. This setting allows to bind the different
  # instances to the same port. Default this setting to true if the number or process
  # is grater than 1.
  #
  # > Read more about Linux PORT REUSE https://lwn.net/Articles/542629/
  #
  settings.port_reuse = true
  #
  #
  # Process Count: This will enable Amber to be used in cluster mode,
  # spinning an instance for each number of process specified here.
  # Rule of thumb, always leave at least 1 core available for system processes/resources.
  #
  # settings.process_count = System.cpu_count.to_i32
  #
  #
  # PORT: This is the port that you're application will run on. Examples would be (80, 443, 3000, 8080)
  #
  settings.port = ENV["PORT"].to_i if ENV["PORT"]?
  #
  #
  # Redis URL: Redis is an in memory key value storage. Amber utilizes redis as
  # a storing option for session information.
  #
  # settings.redis_url = ENV["REDIS_URL"] if ENV["REDIS_URL"]?
  #
  #
  # Database URL: This is the database connection string or data file url.
  # The connection string contains the information to establish a connection to the
  # database or the data file. Defaults to the database provider you chose at
  # at app generation.
  #
  # settings.database_url = ENV["DATABASE_URL"] if ENV["DATABASE_URL"]?
  #
  #
  # SSL Key File: The private key is a text file used initially to generate a
  # Certificate Signing Request (CSR), and later to secure and verify connections
  # using the certificate created per that request. The private key is used to create
  # a digital signature as you might imagine from the name, the private key should be
  # ``closely guarded.
  #
  # settings.ssl_key_file = ENV["SSL_KEY_FILE"] if ENV["SSL_KEY_FILE"]?
  #
  #
  # SSL Cert File: This represents the signed certificate file. SSL Certificates are
  # small data files that digitally bind a cryptographic key to an organization's
  # details. When installed on a web server, it activates the padlock and the https
  # protocol and allows secure connections from a web server to a browser.
  #
  # settings.ssl_cert_file = ENV["SSL_CERT_FILE"] if ENV["SSL_CERT_FILE"]?
  #
  #
  # Session: A Hash that specifies the session storage mechanism, expiration and key to be used
  # for the application. The `key` specifies the name of the cookie to be used defaults to
  # "amber.session". The store can be `encrypted_cookie`, `signed_cookie` or `redis`. Expires
  # when set to 0 means this is indefinitely and is expressed in seconds.
  #
  # settings.session = { "key" => "amber.session", "store" => "signed_cookie", "expires" => 0 }
  #
  #
  # Logger: is the logging that Amber and other capable shards in the project will use
  # instead of writing directly to STDOUT. Supply a custom logging to write to syslog, etc.
  #
end
