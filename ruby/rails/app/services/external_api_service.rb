class ExternalApiService
  include HTTParty
  base_uri 'https://jsonplaceholder.typicode.com'
  
  # Fetch external data from a mock API
  def self.fetch_external_posts(limit = 5)
    response = get('/posts', query: { _limit: limit })
    
    return [] unless response.success?
    
    response.parsed_response
  rescue StandardError => e
    Rails.logger.error "Failed to fetch external posts: #{e.message}"
    []
  end
  
  # Fetch a single external post
  def self.fetch_external_post(id)
    response = get("/posts/#{id}")
    
    return nil unless response.success?
    
    response.parsed_response
  rescue StandardError => e
    Rails.logger.error "Failed to fetch external post #{id}: #{e.message}"
    nil
  end
  
  # Create a post on external API (simulated)
  def self.create_external_post(post_data)
    response = post('/posts', body: post_data.to_json, headers: { 'Content-Type' => 'application/json' })
    
    return nil unless response.success?
    
    response.parsed_response
  rescue StandardError => e
    Rails.logger.error "Failed to create external post: #{e.message}"
    nil
  end
  
  # Health check for external service
  def self.health_check
    response = get('/posts/1')
    response.success?
  rescue StandardError
    false
  end
end
