class HttpClient
  DEFAULT_TIMEOUT = 10

  def self.default_timeout
    DEFAULT_TIMEOUT
  end

  def self.get_external_resource(id)
    return { error: "Resource not found" } unless id.to_i == 1

    {
      "id" => 1,
      "title" => "External resource",
      "source" => "mock_external_api",
      "timestamp" => Time.current.utc.iso8601,
    }
  end
end
