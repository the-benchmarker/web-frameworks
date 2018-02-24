
guard :rspec, cmd: 'rspec spec', all_on_start:true do
  watch(%r{^app/(.+)\.rb$})             { 'spec' }
  watch(%r{^spec/(.+)_spec\.rb$})       { 'spec' }

  watch('spec/spec_helper.rb')          { 'spec' }
  watch('config/routes.txt')            { 'spec' }
end
