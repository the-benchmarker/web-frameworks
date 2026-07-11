namespace :setup do
  desc "Setup the Rails workload"
  task :all do
    puts "Setting up Rails workload..."
    
    # Create database
    Rake::Task["db:create"].invoke
    Rake::Task["db:migrate"].invoke
    Rake::Task["db:seed"].invoke
    
    puts "Rails workload setup complete!"
  end
end