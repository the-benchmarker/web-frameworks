# Base class for all application models in Rails
# This inherits from ActiveRecord::Base and provides a common
# ancestor for all application models
class ApplicationRecord < ActiveRecord::Base
  self.abstract_class = true
end
