require 'digest'

class UploadSummary
  def self.call(file:)
    size = file.size
    first_byte = last_byte = nil

    unless size.zero?
      file.rewind
      first_byte = file.read(1).getbyte(0)
      file.tempfile.seek(-1, IO::SEEK_END)
      last_byte = file.read(1).getbyte(0)
    end

    {
      first_byte: first_byte,
      last_byte: last_byte,
      checksum: Digest::SHA256.file(file.tempfile.path).hexdigest
    }
  end
end
