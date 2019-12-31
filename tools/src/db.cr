require "admiral"
require "sqlite3"
require "crustache"
require "yaml"

class App < Admiral::Command
  class InitializeDatabase < Admiral::Command
    def run
      DB.open "sqlite3://./data.db" do |db|
        db.exec "create table languages (id INTEGER PRIMARY KEY AUTOINCREMENT, label text UNIQUE)"
        db.exec "create table frameworks (id INTEGER PRIMARY KEY AUTOINCREMENT, language_id INTEGER, label text, FOREIGN KEY(language_id) REFERENCES languages(id))"
        db.exec "create table metric_keys (id INTEGER PRIMARY KEY AUTOINCREMENT, label text, framework_id INTEGER, FOREIGN KEY(framework_id) REFERENCES frameworks(id))"
        db.exec "create table metric_values (id INTEGER PRIMARY KEY AUTOINCREMENT, metric_id INTEGER, value FLOAT, FOREIGN KEY(metric_id) REFERENCES metrics(id))"
      end
    end
  end

  class ReadmeWriter < Admiral::Command
    def run
      results = {} of String => Hash(String, String | Float64)
      order_by_latency = <<-EOS
SELECT f.id, l.label AS language, f.label AS framework, k.label AS key, sum(v.value/3) AS value, k.label='request:per_second' AS filter 
  FROM frameworks AS f 
  JOIN languages AS l on l.id = f.language_id 
  JOIN metric_keys as k on k.framework_id = f.id
  JOIN metric_values as v on v.metric_id = k.id 
    GROUP BY 1,2,3,4
    ORDER BY 6 desc, 5 desc
EOS
      DB.open "sqlite3://data.db" do |db|
        db.query order_by_latency do |row|
          row.each do
            key = row.read(Int).to_s
            language = row.read(String)
            framework = row.read(String)
            metric = row.read(String)
            value = row.read(Float)
            unless results.has_key?(key)
              results[key] = {} of String => String | Float64
              results[key]["language"] = language
              results[key]["framework"] = framework
              config = YAML.parse(File.read("#{language}/config.yaml"))
              results[key]["language_version"] = config["provider"]["default"]["language"].to_s
              config = YAML.parse(File.read("#{language}/#{framework}/config.yaml"))
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
      c=1
      results.each do |_, row|
        lines << "| %s | %s (%s)| %s (%s) | %s | | |" % [
          c,
          row["language"],
          row["language_version"],
          row["framework"],
          row["framework_version"],
          row["request:per_second"].to_f.trunc.format(delimiter: ' ', decimal_places: 0),
        ]
        c+=1
      end

      path = File.expand_path("../../../README.mustache.md", __FILE__)
      template = Crustache.parse(File.read(path))
      puts Crustache.render template, {"results" => lines}
    end
  end

  register_sub_command init : InitializeDatabase, description "Create database"
  register_sub_command to_readme : ReadmeWriter, description "Update readme with results"

  def run
    puts "help"
  end
end

App.run
