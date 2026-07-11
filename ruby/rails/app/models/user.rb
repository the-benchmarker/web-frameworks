# Category 5: Data Management - Database Integration & ORM
# Production-grade User model with security and validation
class User < ApplicationRecord
  # Security: Prevent mass assignment vulnerabilities
  # In Rails, strong parameters handle this at the controller level
  # This model only exposes name and email via the controller's user_params

  # Validations for data integrity and security
  # Presence validations
  validates :name,
    presence: { message: "Name cannot be blank" },
    length: {
      maximum: 100,
      too_long: "Name must be less than %{count} characters",
    }

  validates :email,
    presence: { message: "Email cannot be blank" },
    format: {
      with: URI::MailTo::EMAIL_REGEXP,
      message: "Email must be a valid email address",
    },
    uniqueness: {
      case_sensitive: false,
      message: "Email has already been taken",
    }

  # Security: Strip whitespace from email before validation
  before_validation :strip_email

  # Security: Normalize email to lowercase for uniqueness
  before_save :downcase_email

  # Callbacks for logging
  after_create :log_user_creation
  before_destroy :log_user_deletion

  private

  # Strip whitespace from email
  def strip_email
    self.email = email.strip if email.present?
  end

  # Convert email to lowercase for consistent uniqueness
  def downcase_email
    self.email = email.downcase if email.present?
  end

  # Log user creation
  def log_user_creation
    Rails.logger.info "User created: id=#{id}, email=#{email}"
  end

  # Log user deletion
  def log_user_deletion
    Rails.logger.info "User deleted: id=#{id}, email=#{email}"
  end
end
