class CacheService
  def self.client
    @client ||= Redis.new(host: ENV['REDIS_HOST'] || 'localhost', port: ENV['REDIS_PORT'] || 6379)
  end
  
  def self.get(key)
    client.get(key)
  rescue Redis::CannotConnectError
    nil
  end
  
  def self.set(key, value, expires_in = 3600)
    client.setex(key, expires_in, value)
  rescue Redis::CannotConnectError
    false
  end
  
  def self.delete(key)
    client.del(key)
  rescue Redis::CannotConnectError
    false
  end
  
  def self.increment(key, by = 1)
    client.incrby(key, by)
  rescue Redis::CannotConnectError
    0
  end
  
  def self.cache_response(key, expires_in: 3600, &block)
    cached = get(key)
    return JSON.parse(cached) if cached.present?
    
    result = block.call
    set(key, result.to_json, expires_in)
    result
  end
end
