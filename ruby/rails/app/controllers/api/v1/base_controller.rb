module Api
  module V1
    class BaseController < ApplicationController
      before_action :authenticate_request
      before_action :set_pagination_headers, only: [:index]
      
      rescue_from ActiveRecord::RecordNotFound, with: :not_found
      rescue_from ActionController::ParameterMissing, with: :bad_request
      
      attr_reader :current_user
      
      private
      
      def authenticate_request
        # For testing purposes, we'll accept a simple token
        # In production, use proper JWT or OAuth2
        auth_header = request.headers['Authorization']
        
        if auth_header && auth_header.start_with?('Bearer ')
          token = auth_header.split(' ').last
          @current_user = User.find_by(token: token) if token.present?
        end
        
        # Allow unauthenticated access for now (for benchmarking)
        # render json: { error: 'Unauthorized' }, status: :unauthorized unless @current_user
      end
      
      def not_found(exception)
        render json: { error: exception.message }, status: :not_found
      end
      
      def bad_request(exception)
        render json: { error: exception.message }, status: :bad_request
      end
      
      def set_pagination_headers
        page = params[:page] || 1
        per_page = params[:per_page] || 20
        
        response.headers['X-Page'] = page.to_s
        response.headers['X-Per-Page'] = per_page.to_s
        response.headers['X-Total-Count'] = '0' # Will be set by specific controllers
      end
      
      def pagination_params
        {
          page: (params[:page] || 1).to_i,
          per_page: [(params[:per_page] || 20).to_i, 100].min
        }
      end
      
      def render_collection(collection, serializer, status: :ok)
        render json: collection, each_serializer: serializer, status: status
      end
      
      def render_resource(resource, serializer, status: :ok)
        render json: resource, serializer: serializer, status: status
      end
    end
  end
end
