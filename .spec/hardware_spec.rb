# frozen_string_literal: true

require_relative '../.tasks/helpers'

RSpec.describe 'memory_kb_from' do
  it 'reads MemTotal in kB' do
    meminfo = "MemTotal:       32506704 kB\nMemFree:         1178304 kB\n"
    expect(memory_kb_from(meminfo)).to eq(32_506_704)
  end

  it 'is nil without a MemTotal line' do
    expect(memory_kb_from('')).to be_nil
    expect(memory_kb_from("MemFree: 1 kB\n")).to be_nil
  end
end

RSpec.describe 'cpu_name_from' do
  it 'reads the first model name' do
    cpuinfo = "processor\t: 0\nmodel name\t: AMD Ryzen 7 5800H with Radeon Graphics\n" \
              "processor\t: 1\nmodel name\t: AMD Ryzen 7 5800H with Radeon Graphics\n"
    expect(cpu_name_from(cpuinfo)).to eq('AMD Ryzen 7 5800H with Radeon Graphics')
  end

  it 'is nil where the kernel prints no model name' do
    expect(cpu_name_from("processor\t: 0\nCPU part\t: 0xd0c\n")).to be_nil
  end
end

RSpec.describe 'host_hardware' do
  it 'describes the machine it runs on' do
    hardware = host_hardware

    expect(hardware[:cpus]).to eq(Etc.nprocessors)
    expect(hardware[:memory]).to be > 0
    expect(hardware[:cpu_name]).not_to be_empty
    expect(hardware[:os]).to eq(Etc.uname)
  end
end
