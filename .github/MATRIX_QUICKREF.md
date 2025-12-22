# Quick Reference: Matrix System

## Common Operations

### View Current Matrix

```bash
# For all frameworks
export FILES='["data.json"]'
bundle exec rake ci:matrix | jq

# For specific language
export FILES='["python/fastapi/config.yaml"]'
bundle exec rake ci:matrix | jq

# Count total matrix jobs
export FILES='["data.json"]'
bundle exec rake ci:matrix | jq '.include | length'
```

### Add a New Language

1. Edit `.github/matrix-config.yaml`:
   ```yaml
   runtimes:
     mylang:
       - "1.0"
       - "2.0"
   
   default_runtimes:
     mylang: "2.0"
   ```

2. Create directory structure:
   ```bash
   mkdir -p mylang/framework1
   ```

3. Add language config `mylang/config.yaml`:
   ```yaml
   language:
     version: 2.0
     engines:
       engine1:
         command: /path/to/app
   
   framework:
     engines:
       - engine1
     files:
       - "**/*.ml"
   ```

### Add a New Framework

1. Create framework directory:
   ```bash
   mkdir -p <language>/<framework>
   ```

2. Create `<language>/<framework>/config.yaml`:
   ```yaml
   framework:
     website: example.com
     version: 1.0
     engines:
       - engine1
   ```

3. Add framework files and test:
   ```bash
   bundle exec rake config
   export FRAMEWORK=<language>/<framework>
   make -f $FRAMEWORK/.Makefile build
   ```

### Add Runtime Versions

Edit `.github/matrix-config.yaml`:

```yaml
runtimes:
  python:
    - "3.12"
    - "3.13"  # Add this
```

### Override Framework Runtime

Edit `.github/matrix-config.yaml`:

```yaml
framework_runtime_overrides:
  python/myframework:
    - "3.11"
    - "3.12"
```

### Exclude Incompatible Combinations

Edit `.github/matrix-config.yaml`:

```yaml
exclusions:
  - language: python
    framework: old-framework
    runtime: "3.13"
```

### Configure Caching

Edit `.github/matrix-config.yaml`:

```yaml
cache:
  cache_paths:
    mylang:
      - ~/.mylang/cache
      - ~/.mylang/packages
```

### Trigger Full Runtime Testing

On a PR, modify any file matching runtime trigger paths:

```yaml
runtime_trigger_paths:
  python:
    - "python/*/Dockerfile"
    - "python/config.yaml"
```

Or set for all frameworks:

```yaml
optimization:
  pr_default_runtime_only: false
```

### Test Specific Runtime Locally

Set the runtime version in your Dockerfile and build:

```dockerfile
FROM python:3.13-slim
# ... rest of Dockerfile
```

```bash
bundle exec rake config
export FRAMEWORK=python/fastapi
make -f $FRAMEWORK/.Makefile build
```

## Configuration Locations

| What | Where |
|------|-------|
| Runtime versions | `.github/matrix-config.yaml` → `runtimes` |
| Default runtimes | `.github/matrix-config.yaml` → `default_runtimes` |
| Cache paths | `.github/matrix-config.yaml` → `cache.cache_paths` |
| Optimization settings | `.github/matrix-config.yaml` → `optimization` |
| Matrix generation logic | `.tasks/ci.rake` |
| GitHub Actions workflow | `.github/workflows/ci.yml` |
| Language config | `<language>/config.yaml` |
| Framework config | `<language>/<framework>/config.yaml` |

## Useful Commands

```bash
# Generate and view matrix
export FILES='["data.json"]'
bundle exec rake ci:matrix | jq

# Count matrix combinations
export FILES='["data.json"]'
bundle exec rake ci:matrix | jq '.include | length'

# View frameworks for a language
bundle exec rake ci:matrix | jq '.include[] | select(.language == "python")'

# View runtimes being tested
bundle exec rake ci:matrix | jq '.include[] | select(.runtime != null) | {language, runtime}'

# Install dependencies
bundle install --jobs 4 --retry 3

# Configure all frameworks
bundle exec rake config

# Build a framework
export FRAMEWORK=python/fastapi
make -f $FRAMEWORK/.Makefile build

# Test a framework
export LANGUAGE=python FRAMEWORK=fastapi ENGINE=uvicorn
bundle exec rspec .spec
```

## Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `FILES` | Changed files (JSON array) | `'["python/fastapi/config.yaml"]'` |
| `GITHUB_EVENT_NAME` | Event type | `pull_request`, `push` |
| `GITHUB_REF_NAME` | Branch name | `main`, `feature/xyz` |
| `FRAMEWORK` | Framework path | `python/fastapi` |
| `LANGUAGE` | Language name | `python` |
| `ENGINE` | Engine/server name | `uvicorn` |
| `RUNTIME_VERSION` | Runtime version | `3.12` |
| `DIRECTORY` | Framework directory | `python/fastapi` |

## Matrix Behavior by Event

| Event | Runtime Testing | Trigger |
|-------|----------------|---------|
| Push to main/master | All versions | Automatic |
| Pull request (default) | Default version only | Automatic |
| Pull request (runtime files) | All versions | File changes |
| Manual workflow | As configured | Manual |

## Debugging

### Matrix generation issues

```bash
# Set debug mode
export DEBUG=1
export FILES='["python/fastapi/config.yaml"]'
bundle exec rake ci:matrix
```

### Check framework config

```ruby
# In irb or rake console
require_relative 'Rakefile'
config = get_config_from('python/fastapi')
puts config.inspect
```

### Validate YAML

```bash
# Check matrix config
ruby -ryaml -e "puts YAML.safe_load(File.read('.github/matrix-config.yaml'), permitted_classes: [Symbol], aliases: true).inspect"

# Check language config
ruby -ryaml -e "puts YAML.safe_load(File.read('python/config.yaml')).inspect"
```

## GitHub Actions Tips

### View workflow runs
- Go to Actions tab in GitHub
- Filter by workflow name "CI"
- Click on a run to see matrix jobs

### Re-run failed jobs
- Open the failed workflow run
- Click "Re-run failed jobs"

### Manual workflow dispatch
- Go to Actions tab
- Select "CI" workflow
- Click "Run workflow"

## Best Practices

✅ **Do:**
- Test with default runtime first
- Use selective testing for PRs
- Document runtime requirements
- Cache dependencies
- Monitor CI build times

❌ **Don't:**
- Force full matrix on every PR
- Add untested runtime versions
- Skip local testing
- Ignore cache configuration
- Exceed max_parallel_jobs

## Getting Help

1. Check this quick reference
2. Read full documentation in `.github/MATRIX_SYSTEM.md`
3. Review configuration in `.github/matrix-config.yaml`
4. Examine source in `.tasks/ci.rake`
5. Open an issue with details
