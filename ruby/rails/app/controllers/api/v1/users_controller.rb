module Api
  module V1
    class UsersController < BaseController
      before_action :set_user, only: [:show, :update, :destroy]
      
      # GET /api/v1/users
      def index
        users = User.all
        
        # Filtering
        users = users.where(name: params[:name]) if params[:name].present?
        users = users.where('email LIKE ?', "%#{params[:email]}%") if params[:email].present?
        
        # Sorting
        sort_field = params[:sort_by] || 'created_at'
        sort_direction = params[:sort_direction] || 'desc'
        users = users.order("#{sort_field} #{sort_direction}")
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        users = users.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = users.total_count.to_s
        
        render_collection(users, UserSerializer)
      end
      
      # GET /api/v1/users/:id
      def show
        render_resource(@user, UserSerializer)
      end
      
      # POST /api/v1/users
      def create
        user = User.new(user_params)
        
        if user.save
          render_resource(user, UserSerializer, status: :created)
        else
          render json: { errors: user.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # PUT /api/v1/users/:id
      def update
        if @user.update(user_params)
          render_resource(@user, UserSerializer)
        else
          render json: { errors: @user.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # DELETE /api/v1/users/:id
      def destroy
        @user.destroy
        head :no_content
      end
      
      # GET /api/v1/users/:id/posts
      def posts
        user = User.find(params[:id])
        posts = user.posts
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        posts = posts.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = posts.total_count.to_s
        
        render_collection(posts, PostSerializer)
      end
      
      # GET /api/v1/users/:id/comments
      def comments
        user = User.find(params[:id])
        comments = user.comments
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        comments = comments.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = comments.total_count.to_s
        
        render_collection(comments, CommentSerializer)
      end
      
      private
      
      def set_user
        @user = User.find(params[:id])
      end
      
      def user_params
        params.require(:user).permit(:name, :email, :token)
      end
    end
  end
end
