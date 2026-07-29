use reinhardt::app_config;

pub mod urls;
pub mod views;

#[app_config(name = "benchmark", label = "benchmark")]
pub struct BenchmarkConfig;
