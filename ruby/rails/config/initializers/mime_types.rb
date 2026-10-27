# Use Rails' JSON parser for JSON and its legacy aliases, excluding +json types.
Mime::Type.unregister :json
Mime::Type.register 'application/json', :json, %w[text/x-json application/jsonrequest]
