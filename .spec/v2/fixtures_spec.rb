require_relative '../spec_helper'

RSpec.describe 'shared request fixtures' do
  {
    'deserialization.json' => 'b143133183926f4aa28f5b4937c3f632467d43431fb7cb61d06f9c4c80923bcd',
    'upload.bin' => '1f91053dcf43206eb082c0962785d35d86d4f629345f8bff25be7394416db908'
  }.each do |filename, checksum|
    it "pins the SHA-256 checksum of #{filename}" do
      expect(Digest::SHA256.file(File.join(HttpHelpers::ROOT, filename)).hexdigest).to eq(checksum)
    end
  end

  it 'provides exactly 100 JSON items and the expected ID sum' do
    expect(JSON.parse(fixture_body).keys).to eq(['items'])
    expect(deserialization_summary(fixture_items)).to eq('count' => 100, 'checksum' => 4950)
  end

  it 'provides exactly 4096 upload bytes' do
    expect(upload_bytes.bytesize).to eq(4096)
  end
end
