class PostSerializer
  include FastJsonapi::ObjectSerializer
  
  attributes :title, :content, :created_at
  
  belongs_to :user, serializer: UserSerializer
  has_many :comments, serializer: CommentSerializer
  
  attribute :comment_count do |post|
    post.comments.count
  end
  
  attribute :excerpt do |post|
    post.content.truncate(50)
  end
end
