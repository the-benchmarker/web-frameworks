require "yaml"
require "admiral"
require "pg"
require "crustache"

alias Data = Hash(String, String)

class Merger
  alias ConfigHash = Hash(YAML::Any, YAML::Any)

  def initialize(params : ConfigHash)
    @params = params
  end

  def merge(other = {} of String => String)
    @params.try do |params|
      other = params.merge(other)
    end
    return other
  end
end

class App < Admiral::Command
  class ReadmeWriter < Admiral::Command
    def run
      frameworks = {} of Int32 => Data
      DB.open(ENV["DATABASE_URL"]) do |db|
        db.query("SELECT f.id as framework, l.label, f.label FROM frameworks AS f JOIN languages AS l ON l.id = f.language_id") do |row|
          row.each do
            id = row.read(Int).to_i32
            language = row.read(String)
            framework = row.read(String)
            if framework == "vapor"
              subdir = "vapor-framework"
            elsif framework == "swifter"
              subdir = "swifter-framework"
            else
              subdir = framework
            end
            language_config = YAML.parse(File.read("#{language}/config.yaml"))
            merger = Merger.new(language_config.as_h)
            framework_config = YAML.parse(File.read("#{language}/#{subdir}/config.yaml"))
            config = merger.merge(framework_config.as_h)

            if config["framework"].as_h.has_key?("github")
              website = "https://github.com/#{config["framework"]["github"].to_s}"
            else
              website = "https://#{config["framework"]["website"].to_s}"
            end

            frameworks[id] = {
              "language" => language, "language_version" => config["provider"]["default"]["language"].to_s,
              "framework" => framework, "framework_version" => config["framework"]["version"].to_s, "framework_website" => website,
            }
          end
        end

        query = <<-EOS
  SELECT f.id as framework, c.level::integer, avg(v.value)
  FROM values AS v
    JOIN metrics AS m ON m.value_id = v.id
    JOIN frameworks AS f ON f.id = m.framework_id
    JOIN concurrencies AS c ON c.id = m.concurrency_id
      GROUP BY 1,2
  EOS

        db.query query do |row|
          row.each do
            id = row.read(Int).to_i32
            level = row.read(Int)
            value = row.read(Float)
            frameworks[id]["concurrency_#{level}"] = value.to_s
          end
        end
      end

      lines = [
        "|    | Language | Framework | Speed (64) | Speed (256) | Speed (512) | Speed (1024) |  Speed (2048) |",
        "|----|----------|-----------|-----------:|------------:|------------:|-------------:|--------------:|",
      ]
      c = 1
      sorted = frameworks.values.sort do |rank0, rank1|
        rank1["concurrency_64"].to_f <=> rank0["concurrency_64"].to_f
      end
      sorted.each do |row|
        lines << "| %s | %s (%s)| [%s](%s) (%s) | %s | %s | %s | %s | %s |" % [
          c,
          row["language"],
          row["language_version"],
          row["framework"],
          row["framework_website"],
          row["framework_version"],
          row["concurrency_64"].to_f.trunc.format(delimiter: ' ', decimal_places: 0),
          row["concurrency_256"].to_f.trunc.format(delimiter: ' ', decimal_places: 0),
          row["concurrency_512"].to_f.trunc.format(delimiter: ' ', decimal_places: 0),
          row["concurrency_1024"]?.try &.to_f.trunc.format(delimiter: ' ', decimal_places: 0),
          row["concurrency_2048"]?.try &.to_f.trunc.format(delimiter: ' ', decimal_places: 0),
        ]
        c += 1
      end

      path = File.expand_path("../../../README.mustache.md", __FILE__)
      template = Crustache.parse(File.read(path))
      STDOUT.print Crustache.render template, {"results" => lines, "date": Time.local.to_s("%Y-%m-%d")}
    end
  end

  class ClearResults < Admiral::Command
    def run
      DB.open(ENV["DATABASE_URL"]) do |db|
        db.exec "DELETE FROM metrics;"
        db.exec "DELETE FROM values;"
      end
    end
  end

  define_help
  register_sub_command to_readme : ReadmeWriter, description "Update readme with results"
  register_sub_command clear : ClearResults, description "Clears the data from past runs"

  def run
    puts help
  end
end

App.run
