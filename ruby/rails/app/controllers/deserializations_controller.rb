class DeserializationsController < ApplicationController
  use RequestMediaType, 'application/json', only: :create
  use JsonBodyLength, only: :create

  before_action :set_items, only: :create

  def create
    render json: DeserializationSummary.call(items: @items)
  end

  private

  def set_items
    return bad_request unless request.query_string.empty?

    @items = request.request_parameters['items']
    return bad_request unless @items.is_a?(Array) && @items.length == 100

    bad_request unless @items.all? { |item| item.is_a?(Hash) && valid_id?(item['id']) }
  end

  def valid_id?(id)
    (id.is_a?(Integer) || id.is_a?(Float)) && id.between?(0, 1_000_000) && id == id.to_i
  end
end
