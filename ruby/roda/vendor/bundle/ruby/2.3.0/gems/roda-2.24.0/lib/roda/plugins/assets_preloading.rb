# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The assets_preloading plugin generates html tags or a header value
    # to facilitate browser preloading of your assets. This allows
    # compatible browsers to fetch assets before they are required,
    # streamlining page rendering.
    #
    # For a list of compatible browsers, see
    # http://caniuse.com/#search=link-rel-preload
    #
    # The plugin provides two functions - preload_assets_link_header and
    # preload_assets_link_tags. The resulting preloading should be
    # identical, it is up to you which system you prefer.
    #
    # preload_assets_link_header returns a string suitable for populating
    # the response Link header:
    #
    #   response.headers['Link'] = preload_assets_link_header(:css)
    #   # Link header will now contain something like:
    #   # </assets/app.css>;rel="preload";as="style"
    #
    # preload_assets_link_tags returns a string to drop into your
    # templates containing link tags:
    #
    #   preload_assets_link_tags(:css)
    #   # returns <link href="/assets/app.css" rel="preload" as="style">
    #
    # Note that these link tags are different to the usual asset
    # declarations in markup; this will only instruct a compatible browser
    # to fetch the file and cache it for later; the browser will not parse
    # the asset until it encounters a traditional link or script tag.
    #
    # You must still setup and link to your assets as you did previously.
    #
    # Both functions can be passed any combination of asset types or
    # asset groups, as multiple arguments:
    #
    #   # generate tags for css assets and the app js asset group
    #   preload_assets_link_tags(:css, [:js, :app], [:js, :bar])
    #
    #   # generate Link header for css assets and js asset groups app and bar
    #   preload_assets_link_header(:css, [:js, :app])
    #
    module AssetsPreloading
      TYPE_AS = {
        :css => 'style'.freeze,
        :js => 'script'.freeze,
      }.freeze
      COMMA = ",".freeze
      NEWLINE= "\n".freeze

      # Depend on the assets plugin, as we'll be calling some functions in it.
      def self.load_dependencies(app)
        app.plugin :assets
      end

      module InstanceMethods
        # Return a string of <link> tags for the given asset
        # types/groups.
        def preload_assets_link_tags(*args)
          _preload_assets_array(args).map{|path, as| "<link href=\"#{h(path)}\" rel=\"preload\" as=\"#{as}\">"}.join(NEWLINE)
        end

        # Return a string suitable for a Link header for the
        # given asset types/groups.
        def preload_assets_link_header(*args)
          _preload_assets_array(args).map{|path, as| "<#{path}>;rel=preload;as=#{as}"}.join(COMMA)
        end

        private

        # Return an array of paths/as pairs for the given asset
        # types and/or groups.
        def _preload_assets_array(assets)
          assets.map do |type|
            paths = assets_paths(type)
            type = type[0] if type.is_a?(Array)
            as = TYPE_AS[type]

            paths.map{|path| [path, as]}
          end.flatten(1)
        end
      end
    end

    register_plugin(:assets_preloading, AssetsPreloading)
  end
end
