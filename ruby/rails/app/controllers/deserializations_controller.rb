require "digest"

class DeserializationsController < ApplicationController
  before_action :set_items, only: :create

  def create
    values = @items.map { |item| item[:value] }
    render json: { count: @items.length, checksum: Digest::SHA256.hexdigest(values.join("\n")) }
  end

  private

  def set_items
    raw_items = params[:items]
    return head :bad_request unless raw_items.is_a?(Array)

    @items = params.expect(items: [[:value]])
    head :bad_request unless @items.length == raw_items.length &&
                             @items.all? { |item| item[:value].is_a?(String) }
  end
end
