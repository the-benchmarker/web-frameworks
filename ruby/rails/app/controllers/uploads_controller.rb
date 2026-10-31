require 'digest'

class UploadsController < ApplicationController
  use RequireMediaType, 'multipart/form-data', only: :create
  before_action :set_upload_parameters, only: :create

  def create
    first_byte = last_byte = nil

    unless @file.size.zero?
      @file.rewind
      first_byte = @file.read(1).getbyte(0)
      @file.tempfile.seek(-1, IO::SEEK_END)
      last_byte = @file.read(1).getbyte(0)
    end

    render json: {
      first_byte: first_byte,
      last_byte: last_byte,
      checksum: Digest::SHA256.file(@file.tempfile.path).hexdigest
    }
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
