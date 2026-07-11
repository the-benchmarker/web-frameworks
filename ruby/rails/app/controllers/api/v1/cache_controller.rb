module Api
  module V1
    class CacheController < BaseController
      # GET /api/v1/cache/health
      def health
        begin
          CacheService.client.ping
          render json: {
            status: 'ok',
            service: 'Redis',
            timestamp: Time.current.iso8601
          }
        rescue => e
          render json: {
            status: 'error',
            error: e.message,
            service: 'Redis',
            timestamp: Time.current.iso8601
          }, status: :service_unavailable
        end
      end
      
      # POST /api/v1/cache/set
      def set
        key = params[:key]
        value = params[:value]
        expires_in = params[:expires_in] || 3600
        
        if key.blank? || value.blank?
          return render json: { error: 'key and value are required' }, status: :bad_request
        end
        
        success = CacheService.set(key, value, expires_in.to_i)
        
        if success
          render json: {
            message: 'Cache set successfully',
            key: key,
            expires_in: expires_in
          }, status: :created
        else
          render json: { error: 'Failed to set cache' }, status: :service_unavailable
        end
      end
      
      # GET /api/v1/cache/get
      def get
        key = params[:key]
        
        if key.blank?
          return render json: { error: 'key is required' }, status: :bad_request
        end
        
        value = CacheService.get(key)
        
        if value
          render json: {
            key: key,
            value: value,
            found: true
          }
        else
          render json: {
            key: key,
            value: nil,
            found: false
          }, status: :not_found
        end
      end
      
      # DELETE /api/v1/cache/delete
      def delete
        key = params[:key]
        
        if key.blank?
          return render json: { error: 'key is required' }, status: :bad_request
        end
        
        success = CacheService.delete(key)
        
        if success
          render json: { message: 'Cache deleted successfully', key: key }
        else
          render json: { error: 'Failed to delete cache' }, status: :service_unavailable
        end
      end
      
      # POST /api/v1/cache/increment
      def increment
        key = params[:key]
        by = params[:by] || 1
        
        if key.blank?
          return render json: { error: 'key is required' }, status: :bad_request
        end
        
        new_value = CacheService.increment(key, by.to_i)
        
        render json: {
          key: key,
          new_value: new_value,
          incremented_by: by
        }
      end
      
      # GET /api/v1/cache/stats
      def stats
        begin
          info = CacheService.client.info
          
          render json: {
            stats: {
              connected_clients: info['connected_clients'],
              used_memory: info['used_memory'],
              used_memory_human: info['used_memory_human'],
              total_commands_processed: info['total_commands_processed'],
              uptime_in_seconds: info['uptime_in_seconds']
            },
            timestamp: Time.current.iso8601
          }
        rescue => e
          render json: { error: e.message }, status: :service_unavailable
        end
      end
    end
  end
end
