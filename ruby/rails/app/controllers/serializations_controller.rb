class SerializationsController < ApplicationController
  before_action :set_seed, only: :show

  def show
    render json: SerializedItems.call(seed: @seed)
  end

  private

  def set_seed
    self.params = request.query_parameters
    @seed = params.expect(:seed)
    return bad_request unless validate_seed?(@seed)

    # Reject repeated seeds and additional query parameters before keys collapse.
    pairs = ActionDispatch::QueryParser.each_pair(request.query_string)
    bad_request unless pairs.one?
  end

  def validate_seed?(seed)
    seed.is_a?(String) && seed.match?(/\A[a-z]{8}\z/)
  end
end
