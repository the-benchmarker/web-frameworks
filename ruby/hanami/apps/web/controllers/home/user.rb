module Web::Controllers::Home
  class User
    include Web::Action

    def call(params)
      self.body = params[:id]
    end
  end
end
