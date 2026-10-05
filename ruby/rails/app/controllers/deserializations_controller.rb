class DeserializationsController < ApplicationController
  before_action :set_items, only: :create

  def create
    render json: DeserializationSummary.call(items: @items).to_json
  end

  private

  def set_items
    raw_items = params[:items]
    return head :bad_request unless raw_items.is_a?(Array)

    return @items = [] if raw_items.empty?

    @items = params.expect(items: [[:value]])
    head :bad_request unless @items.length == raw_items.length &&
                             @items.all? { |item| item[:value].is_a?(String) }
  end
end
