# Migration Guide: Nested CI Matrix System

## Overview

This guide helps you migrate from the previous single-level matrix system to the new three-level nested matrix system that includes runtime versioning.

## What Changed

### Before (Single-level Matrix)
```json
{
  "include": [
    {
      "language": "python",
      "framework": "fastapi",
      "directory": "python/fastapi",
      "engine": "uvicorn"
    }
  ]
}
```

### After (Three-level Matrix with Runtimes)
```json
{
  "include": [
    {
      "language": "python",
      "framework": "fastapi",
      "directory": "python/fastapi",
      "engine": "uvicorn",
      "runtime": "3.12"
    },
    {
      "language": "python",
      "framework": "fastapi",
      "directory": "python/fastapi",
      "engine": "uvicorn",
      "runtime": "3.13"
    }
  ]
}
```

## Key Changes

### 1. Matrix Configuration File

**New file**: `.github/matrix-config.yaml`

This centralized configuration file defines:
- Runtime versions for each language
- Default runtime versions
- Optimization settings
- Cache configurations
- Exclusions and overrides

**Action required**: None for existing frameworks, but you can customize settings as needed.

### 2. Enhanced Matrix Generation

**File**: `.tasks/ci.rake`

The `ci:matrix` task now:
- Supports runtime versioning
- Implements selective testing
- Applies optimization rules
- Handles exclusions

**Action required**: None, backward compatible with existing frameworks.

### 3. Updated GitHub Actions Workflow

**File**: `.github/workflows/ci.yml`

The workflow now:
- Sets up language-specific runtimes
- Passes runtime version to build/test steps
- Includes caching for dependencies
- Supports both push and pull_request events

**Action required**: None, backward compatible.

## Migration Steps

### For Framework Maintainers

#### Step 1: Review Default Runtime

Your framework will now be tested with the default runtime version defined in `.github/matrix-config.yaml`.

Check the default for your language:
```yaml
default_runtimes:
  python: "3.12"
  javascript:
    node: "20"
  java: "21"
  # ... etc
```

**Action**: Verify your framework works with this version.

#### Step 2: Test with Multiple Runtimes (Optional)

If you want to test against multiple runtime versions, your framework will automatically use all versions defined in the `runtimes` section on main/master branches.

**Action**: Test your framework with other runtime versions locally:
```bash
# For Python example
docker build --build-arg PYTHON_VERSION=3.13 ...
```

#### Step 3: Add Runtime-Specific Overrides (If Needed)

If your framework only works with specific runtime versions:

Edit `.github/matrix-config.yaml`:
```yaml
framework_runtime_overrides:
  python/myframework:
    - "3.11"
    - "3.12"
```

**Action**: Add overrides only if necessary.

#### Step 4: Exclude Incompatible Combinations (If Needed)

If specific runtime/engine combinations don't work:

Edit `.github/matrix-config.yaml`:
```yaml
exclusions:
  - language: python
    framework: myframework
    runtime: "3.13"
    engine: "gunicorn"
```

**Action**: Add exclusions only if necessary.

### For CI/CD Administrators

#### Step 1: Verify Matrix Configuration

Review `.github/matrix-config.yaml` and adjust:
- Runtime versions
- Default runtimes
- Optimization settings
- Cache paths

```bash
# View current matrix
export FILES='["data.json"]'
bundle exec rake ci:matrix | jq
```

#### Step 2: Test Matrix Generation

Test the new matrix generation locally:

```bash
# Test with specific files
export FILES='["python/fastapi/config.yaml"]'
export GITHUB_EVENT_NAME=pull_request
bundle exec rake ci:matrix | jq

# Test with full matrix
export FILES='["data.json"]'
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '.include | length'
```

#### Step 3: Monitor Initial Runs

After deployment:
1. Monitor GitHub Actions workflow runs
2. Check build times
3. Verify matrix size stays under limit
4. Review success/failure rates

#### Step 4: Tune Optimization Settings

Based on monitoring, adjust settings in `.github/matrix-config.yaml`:

```yaml
optimization:
  max_parallel_jobs: 256  # Adjust based on runner capacity
  selective_testing: true  # Enable/disable
  pr_default_runtime_only: true  # Enable/disable
```

## Backward Compatibility

### Existing Workflows

The new system is **fully backward compatible**:
- Existing frameworks work without changes
- Old matrix entries (without runtime) still work
- Build and test steps handle missing runtime gracefully

### Gradual Adoption

You can adopt runtime versioning gradually:
1. Start with default runtime only
2. Add more runtimes per language
3. Enable full matrix testing
4. Fine-tune with overrides and exclusions

## Rollback Plan

If issues occur, you can rollback to the old system:

### 1. Revert Workflow Changes

Restore `.github/workflows/ci.yml` to previous version:
```bash
git show HEAD~1:.github/workflows/ci.yml > .github/workflows/ci.yml
```

### 2. Revert Matrix Task

Restore `.tasks/ci.rake` to previous version:
```bash
git show HEAD~1:.tasks/ci.rake > .tasks/ci.rake
```

### 3. Remove Configuration File

```bash
rm .github/matrix-config.yaml
```

## Testing the Migration

### Test Scenarios

#### 1. Pull Request with Framework Change
```bash
export FILES='["python/fastapi/app.py"]'
export GITHUB_EVENT_NAME=pull_request
export GITHUB_REF_NAME=feature/test
bundle exec rake ci:matrix | jq
```

**Expected**: Single matrix entry with default runtime.

#### 2. Pull Request with Runtime File Change
```bash
export FILES='["python/fastapi/Dockerfile"]'
export GITHUB_EVENT_NAME=pull_request
export GITHUB_REF_NAME=feature/test
bundle exec rake ci:matrix | jq
```

**Expected**: Multiple matrix entries with all Python runtimes.

#### 3. Push to Main
```bash
export FILES='["python/fastapi/app.py"]'
export GITHUB_EVENT_NAME=push
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq
```

**Expected**: Multiple matrix entries with all runtimes.

#### 4. Full Matrix
```bash
export FILES='["data.json"]'
export GITHUB_REF_NAME=main
bundle exec rake ci:matrix | jq '.include | length'
```

**Expected**: Large matrix with all combinations (up to max_parallel_jobs).

## Common Issues and Solutions

### Issue 1: Matrix Too Large

**Symptom**: More than 256 jobs generated.

**Solution**: 
```yaml
# In .github/matrix-config.yaml
optimization:
  max_parallel_jobs: 256  # Already set
  pr_default_runtime_only: true  # Enable if not already
```

### Issue 2: Wrong Runtime Version

**Symptom**: Tests fail with incompatible runtime.

**Solution**: Add exclusion or override:
```yaml
exclusions:
  - language: python
    framework: problematic-framework
    runtime: "3.13"
```

### Issue 3: Builds Not Using Cache

**Symptom**: Dependencies re-download every time.

**Solution**: Verify cache paths in `.github/matrix-config.yaml` and ensure GitHub Actions setup steps use caching.

### Issue 4: Selective Testing Not Working

**Symptom**: All runtimes tested on every PR.

**Solution**: Check settings:
```yaml
optimization:
  selective_testing: true
  pr_default_runtime_only: true
```

## Performance Comparison

### Before Migration
- ~50-100 matrix jobs per full run
- Single runtime version per language
- No selective testing
- Manual runtime version updates

### After Migration
- Controlled matrix size (up to 256 jobs)
- Multiple runtime versions per language
- Intelligent selective testing
- Centralized runtime configuration
- Automated cache management

## FAQ

**Q: Do I need to update my framework code?**
A: No, the migration is transparent to framework code.

**Q: Will my existing PRs break?**
A: No, the system is backward compatible.

**Q: Can I opt out of runtime versioning?**
A: Yes, don't add your language to the `runtimes` section in matrix-config.yaml.

**Q: How do I force testing all runtimes on a PR?**
A: Modify a runtime trigger file (like a Dockerfile or language config.yaml).

**Q: What if my framework doesn't support all runtime versions?**
A: Use `framework_runtime_overrides` or `exclusions` in matrix-config.yaml.

**Q: Can I test new runtime versions before they're released?**
A: Yes, add them to the `runtimes` section with a pre-release tag.

## Next Steps

After successful migration:

1. **Monitor and tune**: Watch CI build times and adjust settings
2. **Document requirements**: Update framework READMEs with runtime requirements
3. **Add new runtimes**: As new versions release, add them to matrix-config.yaml
4. **Optimize exclusions**: Identify and exclude incompatible combinations
5. **Share feedback**: Improve the system based on real-world usage

## Getting Help

For migration assistance:
1. Review full documentation in `.github/MATRIX_SYSTEM.md`
2. Check quick reference in `.github/MATRIX_QUICKREF.md`
3. Open an issue with migration-related questions
4. Tag maintainers for guidance

## Rollout Timeline (Suggested)

1. **Week 1**: Deploy migration, monitor main branch
2. **Week 2**: Enable for selected frameworks
3. **Week 3**: Full rollout to all frameworks
4. **Week 4**: Tune optimization settings based on data

## Success Metrics

Track these metrics post-migration:
- Matrix size (should be ≤ max_parallel_jobs)
- Build time (should be similar or faster with caching)
- Cache hit rate (should be >80%)
- Success rate (should be maintained)
- PR feedback time (should be faster with selective testing)
