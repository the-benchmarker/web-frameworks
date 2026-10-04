require "digest"

class UploadsController < ApplicationController
  before_action :set_file, only: :create

  def create
    render json: { sha256: Digest::SHA256.file(@file.tempfile.path).hexdigest }
  end

  private

  def set_file
    @file = params.expect(:file)
    head :bad_request unless @file.is_a?(ActionDispatch::Http::UploadedFile)
  end
end
