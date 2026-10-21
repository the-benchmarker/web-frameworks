class UploadsController < ApplicationController
  before_action :require_multipart, :set_upload_parameters, only: :create

  def create
    render json: UploadSummary.call(file: @file)
  end

  private

  def require_multipart
    unsupported_media_type unless request.media_type == 'multipart/form-data'
  end

  def set_upload_parameters
    file = request.request_parameters['file']
    return bad_request unless file.is_a?(ActionDispatch::Http::UploadedFile)

    @file = file

    # Rack preserves the parsed pairs before Rails collapses repeated names.
    pairs = request.request_parameters_list
    bad_request unless pairs.one? { |key, _| key == 'file' }
  end
end
