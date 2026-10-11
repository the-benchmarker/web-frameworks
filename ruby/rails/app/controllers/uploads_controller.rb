class UploadsController < ApplicationController
  around_action :close_uploaded_file, only: :create
  before_action :require_multipart, :set_upload_parameters, only: :create

  def create
    render json: UploadSummary.call(seed: @seed, file: @file)
  end

  private

  def require_multipart
    unsupported_media_type unless request.media_type == 'multipart/form-data'
  end

  def set_upload_parameters
    input = ActionController::Parameters.new(request.request_parameters)
    seed, file = input.expect(:seed, :file)

    return bad_request unless valid_seed?(seed) && file.is_a?(ActionDispatch::Http::UploadedFile)

    # Rack preserves the parsed pairs before Rails collapses repeated names.
    pairs = request.request_parameters_list
    return bad_request unless pairs.one? { |key, _| key == 'seed' } && pairs.one? { |key, _| key == 'file' }

    @seed = seed
    @file = file
  end

  def close_uploaded_file
    yield
  ensure
    @file&.close
  end
end
