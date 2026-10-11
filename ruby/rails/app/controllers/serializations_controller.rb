class SerializationsController < ApplicationController
  before_action :set_seed, only: :show

  def show
    render json: SerializedItems.call(seed: @seed)
  end

  private

  def set_seed
    @seed = ActionController::Parameters.new(request.query_parameters).expect(:seed)

    # Rails normally keeps the last scalar value. Count decoded query pairs
    # through its parser so repeated seeds cannot silently replace one another.
    seed_count = ActionDispatch::QueryParser.each_pair(request.query_string).count { |key, _| key == 'seed' }
    bad_request unless valid_seed?(@seed) && seed_count == 1
  end
end
