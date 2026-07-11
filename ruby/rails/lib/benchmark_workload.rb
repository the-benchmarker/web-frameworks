# Benchmark workload utilities for Rails

module BenchmarkWorkload
  class << self
    def framework_name
      'Rails'
    end
    
    def framework_version
      Rails.version
    end
    
    def environment
      Rails.env
    end
    
    def available_endpoints
      {
        health: '/',
        legacy_user: '/user/:id',
        legacy_register: '/user',
        api: {
          v1: {
            users: '/api/v1/users',
            posts: '/api/v1/posts',
            comments: '/api/v1/comments',
            external: {
              posts: '/api/v1/external/posts',
              health: '/api/v1/external/health',
              weather: '/api/v1/external/weather',
              rates: '/api/v1/external/rates'
            },
            cache: {
              health: '/api/v1/cache/health',
              set: '/api/v1/cache/set',
              get: '/api/v1/cache/get'
            },
            jobs: {
              process_data: '/api/v1/jobs/process_data',
              send_notification: '/api/v1/jobs/send_notification',
              sync_external: '/api/v1/jobs/sync_external',
              stats: '/api/v1/jobs/stats'
            }
          }
        }
      }
    end
    
    def test_data
      {
        users: 5,
        posts: 10,
        comments: 10
      }
    end
  end
end
