class SerializationsController < ApplicationController
  def show
    query = ActionController::Parameters.new(request.query_parameters)
    count, seed = query.expect(:n, :seed)
    unless count.is_a?(String) && count.match?(/\A[0-9]+\z/) && seed.is_a?(String) && !seed.empty?
      return head :bad_request
    end

    count = count.to_i
    return head :bad_request if count > 1000

    items = Array.new(count) { |index| { id: index, value: "#{seed}:#{index}" } }
    render json: { items: items }
  end
end
