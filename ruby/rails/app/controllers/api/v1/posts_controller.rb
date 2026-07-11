module Api
  module V1
    class PostsController < BaseController
      before_action :set_post, only: [:show, :update, :destroy]
      before_action :set_user_for_post, only: [:create, :update]
      
      # GET /api/v1/posts
      def index
        posts = Post.all.includes(:user, :comments)
        
        # Filtering
        posts = posts.where(user_id: params[:user_id]) if params[:user_id].present?
        posts = posts.where('title LIKE ?', "%#{params[:title]}%") if params[:title].present?
        
        # Sorting
        sort_field = params[:sort_by] || 'created_at'
        sort_direction = params[:sort_direction] || 'desc'
        posts = posts.order("#{sort_field} #{sort_direction}")
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        posts = posts.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = posts.total_count.to_s
        
        render_collection(posts, PostSerializer)
      end
      
      # GET /api/v1/posts/:id
      def show
        render_resource(@post, PostSerializer)
      end
      
      # POST /api/v1/posts
      def create
        post = @user.posts.new(post_params)
        
        if post.save
          render_resource(post, PostSerializer, status: :created)
        else
          render json: { errors: post.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # PUT /api/v1/posts/:id
      def update
        if @post.update(post_params)
          render_resource(@post, PostSerializer)
        else
          render json: { errors: @post.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # DELETE /api/v1/posts/:id
      def destroy
        @post.destroy
        head :no_content
      end
      
      # GET /api/v1/posts/:id/comments
      def comments
        post = Post.find(params[:id])
        comments = post.comments.includes(:user)
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        comments = comments.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = comments.total_count.to_s
        
        render_collection(comments, CommentSerializer)
      end
      
      # GET /api/v1/posts/recent
      def recent
        posts = Post.recent.includes(:user, :comments)
        render_collection(posts, PostSerializer)
      end
      
      private
      
      def set_post
        @post = Post.find(params[:id])
      end
      
      def set_user_for_post
        @user = User.find(params[:user_id]) if params[:user_id].present?
        @user ||= current_user if current_user.present?
      end
      
      def post_params
        params.require(:post).permit(:title, :content, :user_id)
      end
    end
  end
end
