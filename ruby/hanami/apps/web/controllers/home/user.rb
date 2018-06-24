module Web::Controllers::Home
  class User
    include Web::Action

    def call(params)
      params[:id]
    end
  end
end
