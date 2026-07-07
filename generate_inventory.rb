#!/usr/bin/env ruby

require 'fileutils'
require 'yaml'

LANGUAGES = %w[
  c clojure cpp crystal csharp d dart elixir fsharp gleam go gogo guile haskell
  java javascript julia kotlin lua luau nim objc ocaml perl php python r ruby
  rust scala swift v zig
].freeze

def frameworks_in_dir(path)
  return [] unless Dir.exist?(path)
  
  Dir.children(path).select do |child|
    child_path = File.join(path, child)
    # Skip non-directory files and special files
    File.directory?(child_path) && !%w[.git .github .spec .tasks .vscode].include?(child)
  end.sort
end

here = Dir.pwd
inventory = {}

LANGUAGES.each do |lang|
  lang_path = File.join(here, lang)
  frameworks = frameworks_in_dir(lang_path)
  inventory[lang] = frameworks unless frameworks.empty?
end

# Add python/index.py as a special case
if File.exist?(File.join(here, 'python', 'index.py'))
  inventory['python'] ||= []
  inventory['python'] << 'index.py'
end

# Sort languages by number of frameworks (descending)
inventory = inventory.sort_by { |_, frameworks| -frameworks.size }.to_h

total = inventory.values.sum(&:size)

puts "# Complete Framework Inventory"
puts "Generated: #{Time.now}"
puts "Total frameworks: #{total}"
puts ""

inventory.each do |lang, frameworks|
  puts "## #{lang.upcase} (#{frameworks.size} frameworks)"
  frameworks.each do |fw|
    path = File.join(here, lang, fw)
    files = Dir.children(path).select { |f| File.file?(File.join(path, f)) && !f.start_with?('.') }
    puts "- #{fw} (#{files.size} files: #{files.join(', ')})"
  end
  puts ""
end

# Also create a summary
puts "## Summary by Language"
puts ""
puts "| Language | Framework Count |"
puts "|----------|----------------|"
inventory.each do |lang, frameworks|
  puts "| #{lang} | #{frameworks.size} |"
end
puts ""
puts "| **Total** | **#{total}** |"
