# frozen_string_literal: true

require("cuba")

Cuba.define do
  on get do
    on root do
      res.write("")
    end
    on "user/:id" do |id|
      res.write(id)
    end
  end
  on post do
    on "user" do |_id|
      res.write("")
    end
  end
end
