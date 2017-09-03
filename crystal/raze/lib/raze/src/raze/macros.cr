require "kilt"

# halt_plain, halt_json, halt_html

# Extends context storage with user defined types.
#
# class User
#   property name
# end
#
# add_context_storage_type(User)
#
macro add_context_storage_type(type)
  {{ HTTP::Server::Context::STORE_MAPPINGS.push(type) }}
end

macro render(filename)
  Kilt.render({{filename}})
end
