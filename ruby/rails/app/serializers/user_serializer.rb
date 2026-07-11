class UserSerializer
  include FastJsonapi::ObjectSerializer
  
  attributes :name, :email, :created_at
  
  has_many :posts, serializer: PostSerializer
  has_many :comments, serializer: CommentSerializer
  
  attribute :post_count do |user|
    user.posts.count
  end
  
  attribute :full_name do |user|
    "User: #{user.name}"
  end
end
