class Comment < ApplicationRecord
  belongs_to :user
  belongs_to :post
  
  validates :content, presence: true, length: { maximum: 1000 }
  
  def as_json(options = {})
    super(options.merge(only: [:id, :content, :user_id, :post_id, :created_at]))
  end
end
