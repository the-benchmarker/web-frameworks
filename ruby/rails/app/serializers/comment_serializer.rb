class CommentSerializer
  include FastJsonapi::ObjectSerializer
  
  attributes :content, :created_at
  
  belongs_to :user, serializer: UserSerializer
  belongs_to :post, serializer: PostSerializer
end
