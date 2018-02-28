database = "benchmark_lucky_#{Lucky::Env.name}"

LuckyRecord::Repo.configure do
  if Lucky::Env.production?
    settings.url = LuckyRecord::PostgresURL.build(
      hostname: "localhost",
      database: database
    )
  end
end

LuckyMigrator::Runner.configure do
  settings.database = database
end
