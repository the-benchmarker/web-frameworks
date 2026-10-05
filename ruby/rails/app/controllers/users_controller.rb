class UsersController < ApplicationController
  before_action :set_user, only: :show

  def show
    render plain: @user
  end

  def create
    head :ok
  end

  private

  def set_user
    @user = params.expect(:user)
  end
end
