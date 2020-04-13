module App
  class DefaultController < Shivneri::Controller
    @[DefaultWorker]
    def index
      text_result ""
    end
  end
end
