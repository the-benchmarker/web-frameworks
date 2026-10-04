class SerializationsController < ApplicationController
  before_action :set_serialization_parameters, only: :show

  def show
    items = Array.new(@count) { |index| { id: index, value: "#{@seed}:#{index}" } }
    render json: { items: items }
  end

  private

  def set_serialization_parameters
    count, @seed = params.expect(:n, :seed)
    unless count.is_a?(String) && count.match?(/\A[0-9]+\z/) && @seed.is_a?(String) && @seed.present?
      return head :bad_request
    end

    @count = count.to_i
    head :bad_request if @count > 1000
  end
end
