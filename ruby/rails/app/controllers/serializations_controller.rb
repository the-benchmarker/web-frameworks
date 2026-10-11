class SerializationsController < ApplicationController
  before_action :set_count, :set_seed, only: :show

  def show
    render json: SerializedItems.call(count: @count, seed: @seed)
  end

  private

  def valid_seed?(seed)
    seed.is_a?(String) && seed.match?(/\A[\w]{9}\z/)
  end

  def set_count
    @count = params.expect(:count).to_i

    bad_request unless @count.positive?
  end


  def set_seed
    @seed = params.expect(:seed)

    bad_request unless @seed.match?(/\A[\w]{8}\z/)
  end
end
