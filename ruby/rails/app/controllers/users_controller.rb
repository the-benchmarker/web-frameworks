class UsersController < ApplicationController
  def show
    render plain: params.expect(:user)
  end

  def create
    head :ok
  end
end
