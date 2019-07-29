require "admiral"
require "sqlite3"

# Language (Runtime) 	Framework (Middleware) 	Average 	50th percentile 	90th percentile 	99th percentile 	99.9th percentile 	Standard deviation
# Requests / s 	Throughput

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
      order_by_requests = <<-EOS
SELECT f.id, l.label AS language, f.label AS framework, k.label AS key, sum(v.value/3) AS value, k.label='percentile:fifty' AS filter 
  FROM frameworks AS f 
  JOIN languages AS l on l.id = f.language_id 
  JOIN metric_keys as k on k.framework_id = f.id
  JOIN metric_values as v on v.metric_id = k.id 
    GROUP BY 1,2,3,4
    ORDER BY 6 desc, 5
EOS
      DB.open "sqlite3://data.db" do |db|
        db.query order_by_requests do |row|
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
            end
            results[key][metric] = value
          end
        end
      end
      results.each do |_, row|
        p "| %s | %s | %s | **%.2f** ms | %.2f ms | %.2f ms  | %.2f ms  | %s | %s | %s Mb |" % [
          row["language"],
          row["framework"],
          row["latency:average"],
          row["percentile:fifty"].to_f/1000,
          row["percentile:ninety"].to_f/1000,
          row["percentile:ninety_nine"].to_f/1000,
          row["percentile:ninety_nine_ninety"].to_f/1000,
          row["latency:deviation"],
          row["request:per_second"],
          row["request:bytes"].to_f / row["request:duration"].to_f
        ]
      end
    end
  end

  register_sub_command init : InitializeDatabase, description "Create database"
  register_sub_command to_readme : ReadmeWriter, description "Update readme with results"

  def run
    puts "help"
  end
end

App.run
