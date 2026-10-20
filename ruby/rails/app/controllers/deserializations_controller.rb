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
    self.params = request.request_parameters
    @seed = params.expect(:seed)
    raw_items = params[:items]

    return bad_request unless @seed.is_a?(String) && @seed.match?(/\A[a-z0-9]{8}\z/)
    return bad_request unless raw_items.is_a?(Array) && raw_items.length <= 100

    # Validate the original shape before strong parameters can discard entries.
    return bad_request unless raw_items.all?(ActionController::Parameters)

    # expect treats an empty array as missing; the contract explicitly allows it.
    @items = raw_items.empty? ? [] : params.expect(items: [[:id]])
    valid_items = @items.length == raw_items.length && @items.all? { |item| valid_id?(item[:id]) }
    bad_request unless valid_items
  end

  def valid_id?(id)
    (id.is_a?(Integer) || id.is_a?(Float)) && id.between?(0, 1_000_000) && id == id.to_i
  end
end
