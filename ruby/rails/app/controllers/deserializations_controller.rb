require "digest"

class DeserializationsController < ApplicationController
  def create
    items = params.permit(items: [[:value]])[:items]
    return head :bad_request unless valid_items?(items)

    values = items.map { |item| item[:value] }
    render json: { count: items.length, checksum: Digest::SHA256.hexdigest(values.join("\n")) }
  end

  private

  def valid_items?(items)
    items.is_a?(Array) && items.all? do |item|
      item.is_a?(ActionController::Parameters) && item[:value].is_a?(String)
    end
  end
end
