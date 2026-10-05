require "digest"

class UploadChecksum
  def self.call(file:)
    { sha256: Digest::SHA256.file(file.tempfile.path).hexdigest }
  end
end
