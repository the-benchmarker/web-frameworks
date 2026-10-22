class UploadsController < ApplicationController
  use RequestMediaType, 'multipart/form-data', only: :create

  before_action :set_upload_parameters, only: :create

  def create
    render json: UploadSummary.call(file: @file)
  end

  private

  def set_upload_parameters
    file = request.request_parameters['file']
    return bad_request unless file.is_a?(ActionDispatch::Http::UploadedFile)

    @file = file

    # Rack preserves the parsed pairs before Rails collapses repeated names.
    # Unselected file inputs (filename="") are omitted by Rack.
    pairs = request.request_parameters_list
    bad_request unless pairs.one? { |key, _| key == 'file' }
  end
end
