//! Settings module for server
//!
//! This module provides environment-specific settings configuration using TOML files.
//!
//! ## Configuration Structure
//!
//! Settings are loaded from TOML files in the `settings/` directory:
//! - `base.toml` - Common settings across all environments
//! - `local.toml` - Local development settings
//! - `staging.toml` - Staging environment settings
//! - `production.toml` - Production environment settings
//!
//! ## Priority Order
//!
//! Settings are merged with the following priority (highest to lowest):
//! 1. Environment variables with `REINHARDT_` prefix
//! 2. Environment-specific TOML file (e.g., `production.toml`)
//! 3. Base TOML file (`base.toml`)
//! 4. Default values
//!
//! ## Environment Selection
//!
//! The environment is determined by the `REINHARDT_ENV` environment variable:
//! - `local` or `development` → loads `local.toml`
//! - `staging` → loads `staging.toml`
//! - `production` → loads `production.toml`
//!
//! If `REINHARDT_ENV` is not set, it defaults to `local`.
//!
//! ## Environment Variable Interpolation
//!
//! `TomlFileSource` interpolates `${VAR}` syntax inside TOML string values
//! by default (since reinhardt-web v0.1.0-rc.27). The `${...}` syntax is
//! not valid in non-string TOML literals. Supported forms:
//!
//! - `${VAR}` — required; settings load fails if `VAR` is unset
//! - `${VAR:-default}` — falls back to `default` when `VAR` is unset
//! - `${VAR:?message}` — settings load fails with `message` when `VAR` is unset
//!
//! Interpolated strings are typed-coerced at deserialization time, so
//! `pool_size = "${DB_POOL_SIZE:-10}"` resolves directly to the field's
//! declared Rust type (e.g. `u16`) without manual parsing.

use reinhardt::conf::settings::builder::SettingsBuilder;
use reinhardt::conf::settings::profile::Profile;
use reinhardt::conf::settings::sources::{DefaultSource, HighPriorityEnvSource, TomlFileSource};
use reinhardt::settings;
use std::env;

#[settings(core: CoreSettings | contacts: ContactSettings)]
pub struct ProjectSettings;

/// Get settings based on environment variable
///
/// Reads the REINHARDT_ENV environment variable to determine which settings to load.
/// Defaults to "local" if not set.
///
/// # Examples
///
/// ```no_run
/// use server::config::settings::get_settings;
///
/// let settings = get_settings();
/// ```
///
/// # Panics
///
/// Panics if:
/// - Settings files cannot be read
/// - Settings cannot be deserialized
/// - Required settings are missing
pub fn get_settings() -> ProjectSettings {
    let profile_str = env::var("REINHARDT_ENV").unwrap_or_else(|_| "local".to_string());
    let profile = Profile::parse(&profile_str);

    // Get the project root directory (parent of src/)
    let base_dir = env::current_dir().expect("Failed to get current directory");
    let settings_dir = base_dir.join("settings");

    // Build settings by merging sources in priority order.
    // `build_composed::<T>()` uses `MergeStrategy::Deep` by default, so a
    // single key in `production.toml` overrides only that key — sibling
    // entries inside the same nested table inherit from `base.toml`.
    SettingsBuilder::new()
        .profile(profile)
        // Lowest priority: Default values
        .add_source(DefaultSource::new())
        // Medium priority: Base TOML file
        .add_source(TomlFileSource::new(settings_dir.join("base.toml")))
        // Profile priority: Environment-specific TOML file
        .add_source(TomlFileSource::new(
            settings_dir.join(format!("{}.toml", profile_str)),
        ))
        // Highest priority: explicit process environment overrides
        .add_source(HighPriorityEnvSource::new().with_prefix("REINHARDT_"))
        .build_composed::<ProjectSettings>()
        .unwrap_or_else(|err| {
            panic!("Failed to build/compose settings for profile `{profile_str}`: {err}")
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_settings() {
        // Smoke test: ensures settings load without panic and required fields are present
        let settings = get_settings();
        assert!(
            !settings.core.secret_key.is_empty(),
            "secret_key should be populated from settings sources"
        );
    }
}
