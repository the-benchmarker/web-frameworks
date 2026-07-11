module Api
  module V1
    class CommentsController < BaseController
      before_action :set_comment, only: [:show, :update, :destroy]
      before_action :set_post_for_comment, only: [:create]
      
      # GET /api/v1/comments
      def index
        comments = Comment.all.includes(:user, :post)
        
        # Filtering
        comments = comments.where(post_id: params[:post_id]) if params[:post_id].present?
        comments = comments.where(user_id: params[:user_id]) if params[:user_id].present?
        
        # Sorting
        sort_field = params[:sort_by] || 'created_at'
        sort_direction = params[:sort_direction] || 'desc'
        comments = comments.order("#{sort_field} #{sort_direction}")
        
        # Pagination
        page = pagination_params[:page]
        per_page = pagination_params[:per_page]
        comments = comments.page(page).per(per_page)
        
        response.headers['X-Total-Count'] = comments.total_count.to_s
        
        render_collection(comments, CommentSerializer)
      end
      
      # GET /api/v1/comments/:id
      def show
        render_resource(@comment, CommentSerializer)
      end
      
      # POST /api/v1/comments
      def create
        comment = @post.comments.new(comment_params)
        comment.user = current_user if current_user.present?
        
        if comment.save
          render_resource(comment, CommentSerializer, status: :created)
        else
          render json: { errors: comment.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # PUT /api/v1/comments/:id
      def update
        if @comment.update(comment_params)
          render_resource(@comment, CommentSerializer)
        else
          render json: { errors: @comment.errors.full_messages }, status: :unprocessable_entity
        end
      end
      
      # DELETE /api/v1/comments/:id
      def destroy
        @comment.destroy
        head :no_content
      end
      
      private
      
      def set_comment
        @comment = Comment.find(params[:id])
      end
      
      def set_post_for_comment
        @post = Post.find(params[:post_id]) if params[:post_id].present?
      end
      
      def comment_params
        params.require(:comment).permit(:content, :post_id, :user_id)
      end
    end
  end
end
