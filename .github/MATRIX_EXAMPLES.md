# Matrix System Examples

This document provides practical examples for working with the nested CI matrix system.

## Table of Contents

1. [Testing Specific Combinations](#testing-specific-combinations)
2. [Adding Framework Runtime Overrides](#adding-framework-runtime-overrides)
3. [Excluding Incompatible Combinations](#excluding-incompatible-combinations)
4. [Custom Runtime Trigger Paths](#custom-runtime-trigger-paths)
5. [Testing Locally](#testing-locally)
6. [Advanced Scenarios](#advanced-scenarios)

## Testing Specific Combinations

### Test a Single Framework (PR Mode)

```bash
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=pull_request
export GITHUB_REF_NAME=feature/test
bundle exec rake ci:matrix | jq
```

**Result**: Tests Python FastAPI with default runtime (3.12) and all engines (uvicorn, hypercorn, daphne, granian).

### Test a Single Framework (Main Branch)

```bash
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq
```

**Result**: Tests Python FastAPI with all runtime versions (3.9-3.13) and all engines.

### Test All Frameworks for a Language

```bash
export FILES='["python/config.yaml"]'
export GITHUB_EVENT_NAME=pull_request
export GITHUB_REF_NAME=feature/test
bundle exec rake ci:matrix | jq
```

**Result**: Tests all Python frameworks with all runtime versions (because python/config.yaml is a runtime trigger).

## Adding Framework Runtime Overrides

### Example 1: Legacy Framework

If you have a legacy framework that only works with older Python versions:

**Edit `.github/matrix-config.yaml`:**

```yaml
framework_runtime_overrides:
  python/legacy-framework:
    - "3.9"
    - "3.10"
  # This framework will only test with Python 3.9 and 3.10
```

**Test it:**

```bash
export FILES='["python/legacy-framework/config.yaml"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '.include[] | select(.framework == "legacy-framework")'
```

### Example 2: Cutting-Edge Framework

If you have a framework that requires the latest runtime versions:

```yaml
framework_runtime_overrides:
  python/modern-framework:
    - "3.12"
    - "3.13"
```

### Example 3: JavaScript Engine-Specific Versions

For a JavaScript framework that needs specific Node versions:

```yaml
framework_runtime_overrides:
  javascript/my-framework:
    node:
      - "20"
      - "22"
```

## Excluding Incompatible Combinations

### Example 1: Exclude Specific Runtime

If a framework doesn't work with a specific Python version:

**Edit `.github/matrix-config.yaml`:**

```yaml
exclusions:
  - language: python
    framework: problematic-framework
    runtime: "3.13"
  # This will skip testing problematic-framework with Python 3.13
```

**Verify:**

```bash
export FILES='["data.json"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '.include[] | select(.framework == "problematic-framework" and .runtime == "3.13")'
# Should return empty
```

### Example 2: Exclude Engine + Runtime Combination

If a specific engine doesn't work with a specific runtime:

```yaml
exclusions:
  - language: python
    framework: async-framework
    engine: gunicorn
    runtime: "3.13"
  # gunicorn with Python 3.13 will be skipped for async-framework
```

### Example 3: Exclude All Old Runtimes for a Framework

```yaml
exclusions:
  - language: go
    framework: modern-go-framework
    runtime: "1.21"
  - language: go
    framework: modern-go-framework
    runtime: "1.22"
  # Only Go 1.23 will be tested
```

### Example 4: Exclude Entire Language/Framework Combo

If you want to temporarily disable a framework:

```yaml
exclusions:
  - language: python
    framework: broken-framework
  # All combinations for broken-framework will be skipped
```

## Custom Runtime Trigger Paths

### Example 1: Add Language-Specific Triggers

If you want Docker changes to trigger full runtime testing:

**Edit `.github/matrix-config.yaml`:**

```yaml
runtime_trigger_paths:
  go:
    - "go/*/Dockerfile"
    - "go/*.Dockerfile"
    - "go/config.yaml"
```

**Test it:**

```bash
# Without trigger file
export FILES='["go/gin/main.go"]'
export GITHUB_EVENT_NAME=pull_request
bundle exec rake ci:matrix | jq '.include[] | select(.language == "go") | .runtime'
# Output: "1.23" (default only)

# With trigger file
export FILES='["go/config.yaml"]'
export GITHUB_EVENT_NAME=pull_request
bundle exec rake ci:matrix | jq '.include[] | select(.language == "go") | .runtime'
# Output: "1.21", "1.22", "1.23" (all versions)
```

### Example 2: Framework-Specific Trigger

For a specific framework with complex build:

```yaml
runtime_trigger_paths:
  python:
    - "python/*/Dockerfile"
    - "python/config.yaml"
    - "python/django/*/Dockerfile"  # Django sub-apps
```

## Testing Locally

### Test Matrix Generation

```bash
# Install dependencies
bundle config set --local path 'vendor/bundle'
bundle install

# Test PR mode with single framework
export PATH="$HOME/.local/share/gem/ruby/3.2.0/bin:$PATH"
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=pull_request
export GITHUB_REF_NAME=feature/my-feature
bundle exec rake ci:matrix | jq

# Test main branch mode
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq

# Count total jobs
bundle exec rake ci:matrix | jq '.include | length'
```

### Test Specific Language

```bash
# Test all Python frameworks
export FILES='["data.json"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '.include[] | select(.language == "python") | {framework, engine, runtime}'

# Count Python jobs
bundle exec rake ci:matrix | jq '.include[] | select(.language == "python") | length'
```

### Test Runtimes Distribution

```bash
# See runtime distribution for Python
export FILES='["data.json"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '[.include[] | select(.language == "python") | .runtime] | group_by(.) | map({runtime: .[0], count: length})'
```

## Advanced Scenarios

### Scenario 1: Beta Runtime Testing

Add a beta runtime version for early testing:

```yaml
runtimes:
  python:
    - "3.9"
    - "3.10"
    - "3.11"
    - "3.12"
    - "3.13"
    - "3.14-beta"  # Add beta version

# Don't make it the default yet
default_runtimes:
  python: "3.13"

# Allow failures for beta version in GitHub Actions
# Add to .github/workflows/ci.yml in the test job:
# continue-on-error: ${{ matrix.runtime == '3.14-beta' }}
```

### Scenario 2: Gradual Runtime Rollout

Roll out a new runtime version gradually:

**Week 1: Add to available runtimes (not default)**

```yaml
runtimes:
  go:
    - "1.21"
    - "1.22"
    - "1.23"
    - "1.24"  # New version

default_runtimes:
  go: "1.23"  # Keep old default
```

**Week 2: Test with specific frameworks**

```yaml
framework_runtime_overrides:
  go/gin:
    - "1.23"
    - "1.24"  # Test only gin with new version
```

**Week 3: Make it default**

```yaml
default_runtimes:
  go: "1.24"  # Now everyone tests with it on PRs
```

### Scenario 3: Multi-Architecture Testing

Test different architectures with different runtimes:

```yaml
runtimes:
  cpp:
    - "gcc-12"
    - "gcc-13"
    - "clang-15"
    - "clang-16"

# In exclusions, handle architecture-specific issues
exclusions:
  - language: cpp
    framework: sse-framework
    runtime: "clang-15"  # Doesn't support SSE on this version
```

### Scenario 4: Performance Testing Different Runtimes

Test framework performance across runtimes:

```bash
# Generate matrix for performance comparison
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main

# Get all runtime combinations
bundle exec rake ci:matrix | jq -r '.include[] | "\(.language)/\(.framework) with \(.engine) on runtime \(.runtime)"'
```

### Scenario 5: Dependency Version Testing

Test a framework with different dependency versions using runtime as a proxy:

```yaml
# In framework config, vary dependencies based on runtime
# python/myframework/config.yaml can be templated based on RUNTIME_VERSION

framework_runtime_overrides:
  python/myframework:
    - "3.10"  # Uses older dependencies
    - "3.12"  # Uses current dependencies  
    - "3.13"  # Uses latest dependencies
```

### Scenario 6: Selective Framework Testing

Only test frameworks that changed:

```bash
# In CI, get changed frameworks
changed_files=$(git diff --name-only HEAD~1)
framework_configs=$(echo "$changed_files" | grep "config.yaml$")

export FILES="$(echo $framework_configs | jq -R -s -c 'split("\n") | map(select(length > 0))')"
bundle exec rake ci:matrix | jq
```

## Debugging Tips

### Check What's Being Generated

```bash
# Pretty print the matrix
bundle exec rake ci:matrix | jq '.'

# Count combinations per language
bundle exec rake ci:matrix | jq '.include | group_by(.language) | map({language: .[0].language, count: length})'

# Show unique runtimes
bundle exec rake ci:matrix | jq '[.include[].runtime] | unique'

# Find frameworks without runtimes
bundle exec rake ci:matrix | jq '.include[] | select(.runtime == null)'
```

### Verify Exclusions Work

```bash
# Generate full matrix
export FILES='["data.json"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main

# Check if excluded combinations appear
bundle exec rake ci:matrix | jq '.include[] | select(.language == "python" and .framework == "excluded-framework" and .runtime == "3.9")'
# Should be empty if exclusion is working
```

### Test Configuration Changes

```bash
# Before changing matrix-config.yaml
bundle exec rake ci:matrix | jq '.include | length' > /tmp/before.txt

# After changes
bundle exec rake ci:matrix | jq '.include | length' > /tmp/after.txt

# Compare
diff /tmp/before.txt /tmp/after.txt
```

## Common Patterns

### Pattern 1: Language Migration

When migrating to a new language version:

```yaml
# Phase 1: Add new version
runtimes:
  ruby:
    - "3.1"
    - "3.2"
    - "3.3"
    - "3.4"  # New

# Phase 2: Test frameworks individually
framework_runtime_overrides:
  ruby/sinatra:
    - "3.3"
    - "3.4"

# Phase 3: Make default
default_runtimes:
  ruby: "3.4"

# Phase 4: Remove old versions
runtimes:
  ruby:
    - "3.3"
    - "3.4"
```

### Pattern 2: Framework Deprecation

When deprecating a framework:

```yaml
# Step 1: Exclude from new runtime versions
exclusions:
  - language: python
    framework: deprecated-framework
    runtime: "3.13"

# Step 2: Later, exclude entirely
exclusions:
  - language: python
    framework: deprecated-framework
```

### Pattern 3: Experimental Feature Testing

Test experimental features with specific runtimes:

```yaml
# Use runtime overrides for experimental branches
framework_runtime_overrides:
  python/experimental-framework:
    - "3.13"  # Only test with latest
```

## Performance Optimization

### Reduce Matrix Size

```yaml
# Option 1: Reduce runtime versions
runtimes:
  python:
    - "3.11"  # Remove older versions
    - "3.12"
    - "3.13"

# Option 2: Use stricter PR testing
optimization:
  pr_default_runtime_only: true  # Only test default on PRs

# Option 3: Add more exclusions
exclusions:
  - language: python
    framework: large-framework
    runtime: "3.11"  # Skip testing on older runtimes
```

### Parallel Job Limits

```yaml
# Increase if you have more runners
optimization:
  max_parallel_jobs: 512

# Or decrease to save resources
optimization:
  max_parallel_jobs: 128
```

## Next Steps

- Review [Complete Documentation](.github/MATRIX_SYSTEM.md)
- Check [Quick Reference](.github/MATRIX_QUICKREF.md)
- Read [Migration Guide](.github/MIGRATION_GUIDE.md)
