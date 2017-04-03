class Roda
  module RodaPlugins
    module StaticPathInfo
    end

    # For backwards compatibilty, don't raise an error
    # if trying to load the plugin
    register_plugin(:static_path_info, StaticPathInfo)
  end
end
