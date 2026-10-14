# frozen_string_literal: true

module Root
  def self.on_http(event)
    event.finish
  end
end

module Users
  def self.show(event)
    event.finish(event.path[1..])
  end

  def self.create(event)
    event.finish
  end
end

neorack_resource Users
map '/user', Users
run Root
