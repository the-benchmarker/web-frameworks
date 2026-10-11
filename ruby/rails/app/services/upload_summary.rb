class UploadSummary
  def self.call(seed:, file:)
    size = file.size
    first_byte = last_byte = nil

    unless size.zero?
      file.rewind
      first_byte = file.read(1).getbyte(0)
      file.tempfile.seek(-1, IO::SEEK_END)
      last_byte = file.read(1).getbyte(0)
    end

    {
      seed: seed,
      filename: file.original_filename,
      contentType: file.content_type,
      size: size,
      firstByte: first_byte,
      lastByte: last_byte
    }
  end
end
