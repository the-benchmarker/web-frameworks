class DeserializationsController < ApplicationController
  before_action :require_json, :set_deserialization_parameters, only: :create

  def create
    render json: DeserializationSummary.call(seed: @seed, items: @items)
  end

  private

  def require_json
    unsupported_media_type unless request.media_type == 'application/json'
  end

  def set_deserialization_parameters
    # Use body parameters so query values cannot supply missing JSON fields.
    input = ActionController::Parameters.new(request.request_parameters)
    @seed = input.expect(:seed)
    raw_items = input[:items]

    return bad_request unless valid_seed?(@seed) && raw_items.is_a?(Array) && raw_items.length <= 100

    # expect treats an empty array as missing; the contract explicitly allows it.
    @items = raw_items.empty? ? [] : input.expect(items: [[:id]])
    valid_items = @items.length == raw_items.length && @items.all? do |item|
      item[:id].is_a?(Integer) && item[:id].between?(0, 1_000_000)
    end
    bad_request unless valid_items
  end
end
