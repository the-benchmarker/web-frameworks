module Server
  class UserController < Shivneri::Controller
    @[Worker("POST")]
    @[Route("/")]
    def create
      text_result ""
    end

    @[Worker("GET")]
    @[Route("/{id}")]
    def get_user
      text_result param["id"]
    end
  end
end
