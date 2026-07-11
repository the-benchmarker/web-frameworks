class Post < ApplicationRecord
  belongs_to :user
  has_many :comments, dependent: :destroy
  
  validates :title, presence: true, length: { maximum: 200 }
  validates :content, presence: true
  
  scope :recent, -> { order(created_at: :desc).limit(10) }
  
  def as_json(options = {})
    super(options.merge(only: [:id, :title, :content, :user_id, :created_at]))
  end
end
