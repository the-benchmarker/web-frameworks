# Category 6: API & Integration - HTTP Client Library
# Simple HTTP client service for external API calls

class HttpClient
  include HTTParty
  base_uri 'https://jsonplaceholder.typicode.com'
  
  # Category 6: API & Integration - Request Retries & Timeout Handling
  default_timeout 10
  
  # Get external resource
  def self.get_external_resource(id = 1)
    response = get("/posts/#{id}")
    
    if response.success?
      response.parsed_response
    else
      { error: "Failed to fetch external resource", status: response.code }
    end
  rescue StandardError => e
    { error: e.message }
  end
end
