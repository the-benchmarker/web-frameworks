class SerializationsController < ApplicationController
  before_action :set_seed, only: :show

  def show
    render json: SerializedItems.call(seed: @seed)
  end

  private

  def set_seed
    @seed = request.query_parameters['seed']
    return bad_request unless @seed.is_a?(String) && @seed.match?(/\A[a-z]{8}\z/)

    # Inspect the pairs before Rails collapses repeated query keys.
    bad_request unless ActionDispatch::QueryParser.each_pair(request.query_string).one?
  end
end
