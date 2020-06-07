# frozen_string_literal: true

require("syro")

App =
  Syro.new do
    get do
      res.write("")
    end

    on "user" do
      on :id do
        get do
          res.write(inbox[:id])
        end
      end

      post do
        res.write("")
      end
    end
  end
