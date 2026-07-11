module Api
  module V1
    class ExternalController < BaseController
      # GET /api/v1/external/posts
      def posts
        limit = params[:limit] || 5
        posts = ExternalApiService.fetch_external_posts(limit.to_i)
        
        if posts.any?
          render json: {
            data: posts,
            source: 'jsonplaceholder.typicode.com',
            count: posts.length,
            cached: false
          }
        else
          render json: { error: 'Failed to fetch external posts' }, status: :service_unavailable
        end
      end
      
      # GET /api/v1/external/posts/:id
      def post
        post = ExternalApiService.fetch_external_post(params[:id])
        
        if post
          render json: {
            data: post,
            source: 'jsonplaceholder.typicode.com',
            cached: false
          }
        else
          render json: { error: 'External post not found' }, status: :not_found
        end
      end
      
      # POST /api/v1/external/posts
      def create_post
        post_data = params.require(:post).permit(:title, :body, :userId)
        result = ExternalApiService.create_external_post(post_data)
        
        if result
          render json: {
            data: result,
            message: 'Post created on external service'
          }, status: :created
        else
          render json: { error: 'Failed to create external post' }, status: :service_unavailable
        end
      end
      
      # GET /api/v1/external/health
      def health
        status = ExternalApiService.health_check
        
        render json: {
          service: 'jsonplaceholder.typicode.com',
          healthy: status,
          timestamp: Time.current.iso8601
        }, status: status ? :ok : :service_unavailable
      end
      
      # GET /api/v1/external/weather
      def weather
        # Simulate weather API call
        # In a real implementation, this would call a weather service
        weather_data = {
          temperature: rand(15.0..30.0).round(1),
          humidity: rand(40..90),
          condition: ['sunny', 'cloudy', 'rainy', 'snowy'].sample,
          location: params[:location] || 'New York',
          timestamp: Time.current.iso8601
        }
        
        render json: {
          data: weather_data,
          source: 'mock_weather_api',
          cached: false
        }
      end
      
      # GET /api/v1/external/rates
      def exchange_rates
        # Simulate exchange rate API call
        rates = {
          base: 'USD',
          date: Time.current.strftime('%Y-%m-%d'),
          rates: {
            EUR: rand(0.85..0.95).round(4),
            GBP: rand(0.75..0.85).round(4),
            JPY: rand(100.0..150.0).round(2),
            CAD: rand(1.25..1.45).round(4)
          }
        }
        
        render json: {
          data: rates,
          source: 'mock_exchange_api',
          cached: false
        }
      end
    end
  end
end
