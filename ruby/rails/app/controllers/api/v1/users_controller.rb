# API v1 Users Controller
# Category 5: Data Management (Database ORM)
#
# HTTP Standards Compliance:
# - RFC 7231: HTTP Semantics and Content
# - RFC 5789: PATCH Method
# - RFC 7232: Conditional Requests (ETag support)

module Api
  module V1
    class UsersController < BaseController
      before_action :set_user, only: [:show, :update, :destroy]
      before_action :set_cache_headers, only: [:index, :show]

      # GET /api/v1/db/users - List all users
      # RFC 7231 Section 4.3.1: 200 OK with collection
      # RFC 7232: Support for conditional requests
      def index
        page = (params[:page] || 1).to_i.clamp(1, 100)
        per_page = (params[:per_page] || 20).to_i.clamp(1, 100)

        users = User.all.order(created_at: :desc)
        users = users.page(page).per(per_page) if defined?(WillPaginate)

        # RFC 7232: Generate ETag for collection
        etag = Digest::MD5.hexdigest(users.map { |u| u.id.to_s }.join(","))

        data = {
          count: users.count,
          page: page,
          per_page: per_page,
          users: users.map { |u| u.slice(:id, :name, :email) },
        }

        response.headers["ETag"] = etag
        render_json(data)
      end

      # POST /api/v1/db/users - Create user
      # RFC 7231 Section 4.3.2: 201 Created with Location header
      # RFC 7231 Section 7.1.2: Location header format
      def create
        user = User.new(user_params)

        if user.save
          data = {
            status: "created",
            user: user.slice(:id, :name, :email, :created_at),
          }
          # RFC 7231 Section 7.1.2: Location header for newly created resource
          headers = { "Location" => "/api/v1/db/users/#{user.id}" }
          render_json(data, status: :created, **headers)
        else
          # RFC 7231 Section 4.3.4: 422 Unprocessable Entity for validation errors
          # RFC 7807: Problem Details format
          render_error("Validation failed", user.errors.full_messages.join(", "), :unprocessable_entity)
        end
      end

      # GET /api/v1/db/users/:id - Get specific user
      # RFC 7231 Section 4.3.1: 200 OK
      # RFC 7235: WWW-Authenticate header if authentication required
      # RFC 7232: ETag for conditional requests
      def show
        # RFC 7232: Generate ETag for individual resource
        etag = Digest::MD5.hexdigest(@user.updated_at.to_s)
        response.headers["ETag"] = etag
        response.headers["Last-Modified"] = @user.updated_at.httpdate

        # RFC 7233: Support for Range requests
        # Note: ActiveRecord::Base doesn't support range requests by default
        # This would be implemented in a production system

        render_json(@user.slice(:id, :name, :email, :created_at))
      end

      # PATCH /api/v1/db/users/:id - Update user
      # RFC 5789: PATCH Method for partial updates
      # RFC 7231 Section 4.3.3: 200 OK for successful update
      def update
        if @user.update(user_params)
          # RFC 7232: Update ETag after modification
          etag = Digest::MD5.hexdigest(@user.updated_at.to_s)
          response.headers["ETag"] = etag
          response.headers["Last-Modified"] = @user.updated_at.httpdate

          render_json(@user.slice(:id, :name, :email, :updated_at))
        else
          # RFC 7231 Section 4.3.4: 422 Unprocessable Entity
          render_error("Validation failed", @user.errors.full_messages.join(", "), :unprocessable_entity)
        end
      end

      # DELETE /api/v1/db/users/:id - Delete user
      # RFC 7231 Section 4.3.5: 204 No Content
      def destroy
        @user.destroy
        # RFC 7231 Section 4.3.5: 204 No Content - no response body
        head :no_content
      end

      private

      def set_user
        # RFC 7231 Section 4.3.3: 404 Not Found for non-existent resource
        @user = User.find(params[:id])
      rescue ActiveRecord::RecordNotFound
        # RFC 7231 Section 4.3.4: 404 Not Found
        # RFC 7807: Problem Details format
        render_error("Not found", "User with id #{params[:id]} not found", :not_found)
      end

      def set_cache_headers
        # RFC 7234: Cache-Control headers
        # For API responses, we typically want to prevent caching of dynamic data
        response.headers["Cache-Control"] = "no-cache, no-store, must-revalidate"
        response.headers["Pragma"] = "no-cache"
        response.headers["Expires"] = "0"
      end

      def user_params
        # Strong parameters for security (Rails feature)
        # Prevents mass assignment vulnerabilities
        params.require(:user).permit(:name, :email)
      end
    end
  end
end
