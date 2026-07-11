# Category 5: Data Management - Database Integration & ORM
class User < ApplicationRecord
  # Validations for data integrity
  validates :name, presence: true, length: { maximum: 100 }
  validates :email, presence: true, format: { with: URI::MailTo::EMAIL_REGEXP }
end
