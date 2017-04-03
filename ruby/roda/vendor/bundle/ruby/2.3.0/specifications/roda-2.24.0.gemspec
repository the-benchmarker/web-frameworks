# -*- encoding: utf-8 -*-
# stub: roda 2.24.0 ruby lib

Gem::Specification.new do |s|
  s.name = "roda".freeze
  s.version = "2.24.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0".freeze) if s.respond_to? :required_rubygems_version=
  s.require_paths = ["lib".freeze]
  s.authors = ["Jeremy Evans".freeze]
  s.date = "2017-03-15"
  s.email = ["code@jeremyevans.net".freeze]
  s.extra_rdoc_files = ["README.rdoc".freeze, "MIT-LICENSE".freeze, "CHANGELOG".freeze, "doc/conventions.rdoc".freeze, "doc/release_notes/1.0.0.txt".freeze, "doc/release_notes/1.1.0.txt".freeze, "doc/release_notes/1.2.0.txt".freeze, "doc/release_notes/1.3.0.txt".freeze, "doc/release_notes/2.0.0.txt".freeze, "doc/release_notes/2.1.0.txt".freeze, "doc/release_notes/2.2.0.txt".freeze, "doc/release_notes/2.3.0.txt".freeze, "doc/release_notes/2.4.0.txt".freeze, "doc/release_notes/2.5.0.txt".freeze, "doc/release_notes/2.5.1.txt".freeze, "doc/release_notes/2.6.0.txt".freeze, "doc/release_notes/2.7.0.txt".freeze, "doc/release_notes/2.8.0.txt".freeze, "doc/release_notes/2.9.0.txt".freeze, "doc/release_notes/2.10.0.txt".freeze, "doc/release_notes/2.11.0.txt".freeze, "doc/release_notes/2.12.0.txt".freeze, "doc/release_notes/2.13.0.txt".freeze, "doc/release_notes/2.14.0.txt".freeze, "doc/release_notes/2.15.0.txt".freeze, "doc/release_notes/2.16.0.txt".freeze, "doc/release_notes/2.17.0.txt".freeze, "doc/release_notes/2.18.0.txt".freeze, "doc/release_notes/2.19.0.txt".freeze, "doc/release_notes/2.20.0.txt".freeze, "doc/release_notes/2.21.0.txt".freeze, "doc/release_notes/2.22.0.txt".freeze, "doc/release_notes/2.23.0.txt".freeze, "doc/release_notes/2.24.0.txt".freeze]
  s.files = ["CHANGELOG".freeze, "MIT-LICENSE".freeze, "README.rdoc".freeze, "doc/conventions.rdoc".freeze, "doc/release_notes/1.0.0.txt".freeze, "doc/release_notes/1.1.0.txt".freeze, "doc/release_notes/1.2.0.txt".freeze, "doc/release_notes/1.3.0.txt".freeze, "doc/release_notes/2.0.0.txt".freeze, "doc/release_notes/2.1.0.txt".freeze, "doc/release_notes/2.10.0.txt".freeze, "doc/release_notes/2.11.0.txt".freeze, "doc/release_notes/2.12.0.txt".freeze, "doc/release_notes/2.13.0.txt".freeze, "doc/release_notes/2.14.0.txt".freeze, "doc/release_notes/2.15.0.txt".freeze, "doc/release_notes/2.16.0.txt".freeze, "doc/release_notes/2.17.0.txt".freeze, "doc/release_notes/2.18.0.txt".freeze, "doc/release_notes/2.19.0.txt".freeze, "doc/release_notes/2.2.0.txt".freeze, "doc/release_notes/2.20.0.txt".freeze, "doc/release_notes/2.21.0.txt".freeze, "doc/release_notes/2.22.0.txt".freeze, "doc/release_notes/2.23.0.txt".freeze, "doc/release_notes/2.24.0.txt".freeze, "doc/release_notes/2.3.0.txt".freeze, "doc/release_notes/2.4.0.txt".freeze, "doc/release_notes/2.5.0.txt".freeze, "doc/release_notes/2.5.1.txt".freeze, "doc/release_notes/2.6.0.txt".freeze, "doc/release_notes/2.7.0.txt".freeze, "doc/release_notes/2.8.0.txt".freeze, "doc/release_notes/2.9.0.txt".freeze]
  s.homepage = "http://roda.jeremyevans.net".freeze
  s.licenses = ["MIT".freeze]
  s.required_ruby_version = Gem::Requirement.new(">= 1.8.7".freeze)
  s.rubygems_version = "2.6.10".freeze
  s.summary = "Routing tree web toolkit".freeze

  s.installed_by_version = "2.6.10" if s.respond_to? :installed_by_version

  if s.respond_to? :specification_version then
    s.specification_version = 4

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_runtime_dependency(%q<rack>.freeze, [">= 0"])
      s.add_development_dependency(%q<rake>.freeze, [">= 0"])
      s.add_development_dependency(%q<minitest>.freeze, [">= 5.7.0"])
      s.add_development_dependency(%q<tilt>.freeze, [">= 0"])
      s.add_development_dependency(%q<erubis>.freeze, [">= 0"])
      s.add_development_dependency(%q<erubi>.freeze, [">= 0"])
      s.add_development_dependency(%q<haml>.freeze, [">= 0"])
      s.add_development_dependency(%q<rack_csrf>.freeze, [">= 0"])
      s.add_development_dependency(%q<sass>.freeze, [">= 0"])
      s.add_development_dependency(%q<json>.freeze, [">= 0"])
      s.add_development_dependency(%q<mail>.freeze, [">= 0"])
    else
      s.add_dependency(%q<rack>.freeze, [">= 0"])
      s.add_dependency(%q<rake>.freeze, [">= 0"])
      s.add_dependency(%q<minitest>.freeze, [">= 5.7.0"])
      s.add_dependency(%q<tilt>.freeze, [">= 0"])
      s.add_dependency(%q<erubis>.freeze, [">= 0"])
      s.add_dependency(%q<erubi>.freeze, [">= 0"])
      s.add_dependency(%q<haml>.freeze, [">= 0"])
      s.add_dependency(%q<rack_csrf>.freeze, [">= 0"])
      s.add_dependency(%q<sass>.freeze, [">= 0"])
      s.add_dependency(%q<json>.freeze, [">= 0"])
      s.add_dependency(%q<mail>.freeze, [">= 0"])
    end
  else
    s.add_dependency(%q<rack>.freeze, [">= 0"])
    s.add_dependency(%q<rake>.freeze, [">= 0"])
    s.add_dependency(%q<minitest>.freeze, [">= 5.7.0"])
    s.add_dependency(%q<tilt>.freeze, [">= 0"])
    s.add_dependency(%q<erubis>.freeze, [">= 0"])
    s.add_dependency(%q<erubi>.freeze, [">= 0"])
    s.add_dependency(%q<haml>.freeze, [">= 0"])
    s.add_dependency(%q<rack_csrf>.freeze, [">= 0"])
    s.add_dependency(%q<sass>.freeze, [">= 0"])
    s.add_dependency(%q<json>.freeze, [">= 0"])
    s.add_dependency(%q<mail>.freeze, [">= 0"])
  end
end
