class SerializationsController < ApplicationController
  before_action :set_serialization_parameters, only: :show

  def show
    render json: SerializedItems.call(count: @count, seed: @seed).to_json
  end

  private

  def set_serialization_parameters
    count, @seed = params.expect(:n, :seed)
    unless count.is_a?(String) && count.match?(/\A[0-9]+\z/) && @seed.is_a?(String) && !@seed.empty?
      return head :bad_request
    end

    @count = count.to_i
    head :bad_request unless @count.positive?
    head :bad_request if @count > 1000
  end
end
