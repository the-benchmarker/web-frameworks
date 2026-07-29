//! benchmark application module
//!
//! A RESTful API application

use reinhardt::app_config;

pub mod admin;
pub mod models;
pub mod serializers;
pub mod services;
pub mod urls;
pub mod views;

#[app_config(name = "benchmark", label = "benchmark")]
pub struct BenchmarkConfig;
