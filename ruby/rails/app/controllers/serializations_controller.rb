class SerializationsController < ApplicationController
  before_action :set_seed, only: :show

  def show
    render json: SerializedItems.call(seed: @seed)
  end

  private

  def set_seed
    # Rails collapses duplicate keys; reject extra pairs before extracting the seed.
    return bad_request if request.query_string.include?('&')

    @seed = request.query_parameters['seed']
    bad_request unless @seed.is_a?(String) && @seed.match?(/\A[a-z]{8}\z/)
  end
end
