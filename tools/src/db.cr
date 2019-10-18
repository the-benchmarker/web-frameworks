require "yaml"
require "admiral"
require "pg"
require "crustache"

class App < Admiral::Command
  class ReadmeWriter < Admiral::Command
    def run
      results = {} of String => Hash(String, String | Float64 | Float32)
      order_by_requests = <<-EOS
SELECT f.id, l.label, f.label, k.label, sum(v.value)/3
  FROM values AS v 
  JOIN metrics AS m ON m.value_id = v.id 
  JOIN keys AS k ON v.key_id = k.id 
  JOIN frameworks AS f ON f.id = m.framework_id 
  JOIN languages AS l ON l.id = f.language_id
    GROUP BY 1, 2, 3, 4
      ORDER BY k.label=$1 desc, 5
EOS
      DB.open("postgresql://postgres@localhost/benchmark") do |db|
        db.query order_by_requests, "latency_average" do |row|
          row.each do
            key = row.read(Int).to_s
            language = row.read(String)
            framework = row.read(String)
            metric = row.read(String).to_s
            value = row.read(Float)
            unless results.has_key?(key)
              results[key] = {} of String => (String | Float64 | Float32)
              results[key]["language"] = language
              config = YAML.parse(File.read("#{language}/config.yaml"))
              results[key]["language_version"] = config["provider"]["default"]["language"].to_s
              results[key]["framework"] = framework
              config = YAML.parse(File.read("#{language}/config.yaml"))
              results[key]["language_version"] = config["provider"]["default"]["language"].to_s
              config = YAML.parse(File.read("#{language}/#{framework}/config.yaml"))
              if config["framework"].as_h.has_key?("github")
                website = "https://github.com/#{config["framework"]["github"].to_s}"
              else
                website = "https://#{config["framework"]["website"].to_s}"
              end
              results[key]["framework_website"] = website
              results[key]["framework_version"] = config["framework"]["version"].to_s
            end
            results[key][metric] = value
          end
        end
      end
      lines = [
        "|    | Language | Framework | Speed (`req/s`) | Horizontal scale (parallelism) | Vertical scale (concurrency) |",
        "|----|----------|-----------|----------------:|-------------|-------------|",
      ]
      c = 1
      results.each do |_, row|
        lines << "| %s | %s (%s)| [%s](%s) (%s) | %s | | |" % [
          c,
          row["language"],
          row["language_version"],
          row["framework"],
          row["framework_website"],
          row["framework_version"],
          row["request:per_second"].to_f.trunc.format(delimiter: ' ', decimal_places: 0),
        ]
        c += 1
      end

      path = File.expand_path("../../../README.mustache.md", __FILE__)
      template = Crustache.parse(File.read(path))
      puts Crustache.render template, {"results" => lines}
    end
  end

  register_sub_command to_readme : ReadmeWriter, description "Update readme with results"

  def run
    puts "help"
  end
end

App.run
