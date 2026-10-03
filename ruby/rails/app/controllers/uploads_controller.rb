require "digest"

class UploadsController < ApplicationController
  def create
    file = params.expect(:file)
    return head :bad_request unless file.is_a?(ActionDispatch::Http::UploadedFile)

    render json: { sha256: Digest::SHA256.file(file.tempfile.path).hexdigest }
  end
end
