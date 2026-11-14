class UsersController < ApplicationController
  def show
    render json: { id: params[:id] }
  end

  def create
    head :ok
  end
end
