class UsersController < ApplicationController
  before_action :set_id, only: :show

  def show
    render json: { id: @id }
  end

  def create
    head :ok
  end

  private

  def set_id
    @id = request.path_parameters[:id]
    bad_request unless @id.is_a?(String) && @id.match?(/\A[0-9]{8}\z/)
  end
end
