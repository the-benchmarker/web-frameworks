# Rails API Workload - Contributing Guide

## Overview

This Rails workload is designed to test and demonstrate all major features of the Ruby on Rails framework for API development. It serves as a comprehensive benchmark for evaluating Rails performance across different scenarios.

## Framework Features Tested

### 1. Routing
- **RESTful routes** for CRUD operations on Users, Posts, and Comments
- **Nested routes** for user-specific posts and comments
- **Custom collection and member routes** for additional functionality
- **Namespace routing** using API versioning
- **Route constraints** and parameter validation
- **Catch-all routes** for 404 handling

### 2. Serialization
- **Fast JSON API** for efficient JSON serialization
- **Custom serializers** for complex object relationships
- **Nested associations** in JSON responses
- **Custom attributes** and computed properties
- **Conditional serialization** based on request context

### 3. External Service Integration
- **HTTP client** operations using HTTParty
- **Third-party API** consumption (JSONPlaceholder)
- **Mock external services** for weather and exchange rate data
- **Service objects** pattern for business logic
- **Error handling** for external API failures

### 4. Database and ORM
- **ActiveRecord** for database operations
- **Model associations** (has_many, belongs_to)
- **Database validations** and callbacks
- **Scopes and query methods** for data filtering
- **Transactions** and database constraints
- **Migrations** for schema management

### 5. Caching
- **Redis integration** for key-value caching
- **Cache read/write** operations
- **Cache expiration** and TTL management
- **Incremental operations** (increment, decrement)
- **Cache statistics** monitoring

### 6. Background Jobs
- **Sidekiq integration** for background processing
- **Job queuing** and execution
- **Batch job processing**
- **Job status tracking**
- **Error handling** in background jobs

### 7. Authentication and Security
- **Request authentication** via Authorization header
- **Token-based authentication** support
- **Security headers** and CORS configuration
- **Parameter sanitization** and strong parameters

### 8. Error Handling
- **Exception rescue** at controller level
- **Custom error responses** for different HTTP status codes
- **Error logging** and monitoring
- **404 handling** for unknown routes

### 9. Pagination
- **Kaminari integration** for pagination
- **Page and per_page** parameter handling
- **Pagination headers** in responses
- **Efficient querying** with pagination

### 10. Request/Response Handling
- **JSON API responses** with proper headers
- **Custom headers** for metadata
- **Request logging** and monitoring
- **Response formatting** and status codes

## API Endpoints

### Health and Status

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/` | Health check and API info | JSON with status, version, and available endpoints |

### Legacy Endpoints (for backward compatibility)

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/user/:id` | Get user ID | Plain text with ID |
| POST | `/user` | Register user | 200 OK (empty body) |

### Users API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/v1/users` | List all users (paginated) | JSON array of users |
| GET | `/api/v1/users/:id` | Get single user | JSON with user data |
| POST | `/api/v1/users` | Create new user | JSON with created user |
| PUT | `/api/v1/users/:id` | Update user | JSON with updated user |
| DELETE | `/api/v1/users/:id` | Delete user | 204 No Content |
| GET | `/api/v1/users/:id/posts` | Get user's posts | JSON array of posts |
| GET | `/api/v1/users/:id/comments` | Get user's comments | JSON array of comments |

**Query Parameters for Users:**
- `page` - Page number (default: 1)
- `per_page` - Items per page (default: 20, max: 100)
- `name` - Filter by user name
- `email` - Filter by email (partial match)
- `sort_by` - Field to sort by (default: created_at)
- `sort_direction` - Sort direction (asc/desc, default: desc)

### Posts API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/v1/posts` | List all posts (paginated) | JSON array of posts |
| GET | `/api/v1/posts/:id` | Get single post | JSON with post data |
| POST | `/api/v1/posts` | Create new post | JSON with created post |
| PUT | `/api/v1/posts/:id` | Update post | JSON with updated post |
| DELETE | `/api/v1/posts/:id` | Delete post | 204 No Content |
| GET | `/api/v1/posts/:id/comments` | Get post's comments | JSON array of comments |
| GET | `/api/v1/posts/recent` | Get recent posts (last 10) | JSON array of posts |

**Query Parameters for Posts:**
- `page` - Page number (default: 1)
- `per_page` - Items per page (default: 20, max: 100)
- `user_id` - Filter by user ID
- `title` - Filter by title (partial match)
- `sort_by` - Field to sort by (default: created_at)
- `sort_direction` - Sort direction (asc/desc, default: desc)

### Comments API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/v1/comments` | List all comments (paginated) | JSON array of comments |
| GET | `/api/v1/comments/:id` | Get single comment | JSON with comment data |
| POST | `/api/v1/comments` | Create new comment | JSON with created comment |
| PUT | `/api/v1/comments/:id` | Update comment | JSON with updated comment |
| DELETE | `/api/v1/comments/:id` | Delete comment | 204 No Content |

**Query Parameters for Comments:**
- `page` - Page number (default: 1)
- `per_page` - Items per page (default: 20, max: 100)
- `post_id` - Filter by post ID
- `user_id` - Filter by user ID
- `sort_by` - Field to sort by (default: created_at)
- `sort_direction` - Sort direction (asc/desc, default: desc)

### External Services API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/v1/external/posts` | Fetch posts from external API | JSON with external posts |
| GET | `/api/v1/external/posts/:id` | Fetch single external post | JSON with external post |
| POST | `/api/v1/external/posts` | Create post on external API | JSON with created post |
| GET | `/api/v1/external/health` | Check external service health | JSON with health status |
| GET | `/api/v1/external/weather` | Get mock weather data | JSON with weather data |
| GET | `/api/v1/external/rates` | Get mock exchange rates | JSON with exchange rates |

**Query Parameters for External Posts:**
- `limit` - Number of posts to fetch (default: 5)
- `location` - Location for weather data (default: New York)

### Cache API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| GET | `/api/v1/cache/health` | Check Redis cache health | JSON with health status |
| POST | `/api/v1/cache/set` | Set cache key-value | JSON with operation result |
| GET | `/api/v1/cache/get` | Get cached value | JSON with cached value |
| DELETE | `/api/v1/cache/delete` | Delete cached key | JSON with operation result |
| POST | `/api/v1/cache/increment` | Increment cached value | JSON with new value |
| GET | `/api/v1/cache/stats` | Get Redis statistics | JSON with cache stats |

**Parameters for Cache Operations:**
- `key` - Cache key (required for all operations)
- `value` - Value to cache (required for set)
- `expires_in` - Expiration time in seconds (default: 3600)
- `by` - Increment amount (default: 1)

### Background Jobs API

| Method | Endpoint | Description | Response |
|--------|----------|-------------|----------|
| POST | `/api/v1/jobs/process_data` | Enqueue data processing job | JSON with job info |
| POST | `/api/v1/jobs/send_notification` | Enqueue notification job | JSON with job info |
| POST | `/api/v1/jobs/sync_external` | Enqueue sync job | JSON with job info |
| GET | `/api/v1/jobs/:id/status` | Check job status | JSON with job status |
| GET | `/api/v1/jobs/stats` | Get job queue statistics | JSON with queue stats |
| POST | `/api/v1/jobs/batch` | Enqueue multiple jobs | JSON with job results |

**Request Body for Job Creation:**
- `data` - Payload data for processing job
- `notification` - Notification data for notification job
- `jobs` - Array of job specifications for batch processing

## Why These Endpoints?

### Comprehensive Feature Coverage

This workload includes endpoints that test all major Rails features:

1. **Routing Complexity** - Tests Rails' routing engine with RESTful, nested, and custom routes
2. **Serialization Performance** - Tests JSON serialization speed with complex object graphs
3. **Database Efficiency** - Tests ActiveRecord query generation and execution
4. **External Service Integration** - Tests HTTP client performance and error handling
5. **Caching Impact** - Tests Redis integration and cache hit/miss scenarios
6. **Background Processing** - Tests job queuing overhead and asynchronous processing

### Real-world Scenarios

The endpoints simulate common real-world API scenarios:

- **Social media platform** - Users, Posts, Comments with relationships
- **Data aggregation** - External API consumption and processing
- **Caching layer** - Performance optimization with Redis
- **Async processing** - Background job execution for long-running tasks
- **Administrative endpoints** - Monitoring and health checks

### Performance Characteristics

Each endpoint category targets different performance aspects:

- **CRUD operations** - Baseline performance for standard API operations
- **Nested resources** - Performance impact of complex queries and associations
- **External calls** - Network I/O and external service dependency impact
- **Cache operations** - Memory access vs database access performance
- **Background jobs** - Queue latency and throughput measurement

## Request Examples

### Create User
```bash
curl -X POST http://localhost:3000/api/v1/users \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer your-token" \
  -d '{"user": {"name": "John Doe", "email": "john@example.com"}}'
```

### Get User with Posts
```bash
curl http://localhost:3000/api/v1/users/1/posts \
  -H "Authorization: Bearer your-token"
```

### Create Post
```bash
curl -X POST http://localhost:3000/api/v1/posts \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer your-token" \
  -d '{"post": {"title": "Hello World", "content": "My first post", "user_id": 1}}'
```

### Fetch External Posts
```bash
curl http://localhost:3000/api/v1/external/posts?limit=10
```

### Set Cache
```bash
curl -X POST http://localhost:3000/api/v1/cache/set \
  -H "Content-Type: application/json" \
  -d '{"key": "test", "value": "hello world", "expires_in": 3600}'
```

### Get Cache
```bash
curl http://localhost:3000/api/v1/cache/get?key=test
```

### Enqueue Job
```bash
curl -X POST http://localhost:3000/api/v1/jobs/process_data \
  -H "Content-Type: application/json" \
  -d '{"data": {"action": "process", "priority": "high"}}'
```

## Response Examples

### Successful User Creation
```json
{
  "data": {
    "id": "1",
    "type": "user",
    "attributes": {
      "name": "John Doe",
      "email": "john@example.com",
      "created-at": "2024-01-01T12:00:00.000Z"
    },
    "relationships": {
      "posts": {
        "data": []
      },
      "comments": {
        "data": []
      }
    }
  }
}
```

### External Posts Response
```json
{
  "data": [
    {
      "userId": 1,
      "id": 1,
      "title": "External post title",
      "body": "External post content"
    }
  ],
  "source": "jsonplaceholder.typicode.com",
  "count": 1,
  "cached": false
}
```

### Cache Health Response
```json
{
  "status": "ok",
  "service": "Redis",
  "timestamp": "2024-01-01T12:00:00.000Z"
}
```

### Job Enqueue Response
```json
{
  "message": "Job enqueued successfully",
  "job_type": "process_data",
  "job_id": "abc-123-def-456",
  "payload": {"action": "process", "priority": "high"},
  "status": "queued"
}
```

## Performance Considerations

### N+1 Query Problem
The workload includes endpoints that intentionally demonstrate both good and bad patterns:

- **Bad**: Endpoints that load associations without `includes` to show N+1 impact
- **Good**: Endpoints that use `includes` to preload associations

### Caching Strategies
The cache endpoints demonstrate different caching approaches:

- **Key-value caching** for simple data
- **Expiration-based** cache invalidation
- **Incremental operations** for counters and metrics

### Memory Usage
The workload tests memory efficiency with:

- Large JSON responses with nested associations
- Pagination to limit response sizes
- Streaming responses for large datasets (potential future enhancement)

### Database Load
The database queries test:

- Index usage with filtered queries
- Join efficiency with nested resources
- Query optimization with scopes and custom methods

## Testing Recommendations

When benchmarking this workload, consider testing:

1. **Baseline performance** - Simple health check endpoint
2. **CRUD operations** - User, Post, Comment endpoints
3. **Complex queries** - Nested resource endpoints
4. **External services** - External API endpoints
5. **Cache operations** - Redis read/write performance
6. **Background jobs** - Job enqueue and processing speed
7. **Concurrent requests** - All endpoints under load

## Architecture Decisions

### Why RESTful Routes?
RESTful routes provide a standardized, predictable API structure that makes it easy to understand and test all CRUD operations systematically.

### Why JSON API Serialization?
JSON API provides a standardized format for JSON responses that includes relationships, making it ideal for testing complex data structures and association performance.

### Why External Service Integration?
Testing external service calls measures the framework's ability to handle network I/O, error conditions, and response processing efficiently.

### Why Background Jobs?
Background job processing tests the framework's ability to offload long-running tasks and handle asynchronous operations without blocking the main request/response cycle.

### Why Caching?
Caching tests the framework's integration with external cache stores and the performance impact of cache hits vs misses.

## File Structure

```
ruby/rails/
├── app/
│   ├── controllers/
│   │   ├── application_controller.rb      # Main application controller
│   │   └── api/
│   │       └── v1/
│   │           ├── base_controller.rb     # Base API controller
│   │           ├── users_controller.rb     # User CRUD operations
│   │           ├── posts_controller.rb     # Post CRUD operations
│   │           ├── comments_controller.rb # Comment CRUD operations
│   │           ├── external_controller.rb # External API integration
│   │           ├── cache_controller.rb    # Cache operations
│   │           └── jobs_controller.rb     # Background job operations
│   ├── models/
│   │   ├── user.rb                        # User model
│   │   ├── post.rb                        # Post model
│   │   └── comment.rb                     # Comment model
│   ├── serializers/
│   │   ├── user_serializer.rb             # User JSON serializer
│   │   ├── post_serializer.rb             # Post JSON serializer
│   │   └── comment_serializer.rb          # Comment JSON serializer
│   └── services/
│       ├── external_api_service.rb       # External API service
│       ├── cache_service.rb              # Cache service
│       └── background_job_service.rb      # Background job service
├── config/
│   ├── routes.rb                         # Route definitions
│   ├── application.rb                    # Application configuration
│   └── database.yml                      # Database configuration
├── db/
│   ├── migrate/                          # Database migrations
│   ├── schema.rb                         # Database schema
│   └── seeds.rb                          # Database seeding
├── Gemfile                              # Dependencies
├── Gemfile.lock                         # Dependency lock file
└── CONTRIBUTING.md                       # This file
```

## Dependencies

The workload requires the following major dependencies:

- `rails ~> 8.1.0` - Core Rails framework
- `sqlite3 < 3` - Database adapter
- `fast_jsonapi` - JSON API serialization
- `httparty` - HTTP client for external APIs
- `sidekiq` - Background job processing
- `redis` - Redis client for caching and jobs
- `kaminari` - Pagination

See `Gemfile` for complete dependency list.

## Setup

To set up this workload for testing:

```bash
# Install dependencies
bundle install

# Create database
rails db:create
rails db:migrate

# Seed database (optional)
rails db:seed

# Start server
rails server -p 3000
```

## Contributing

When adding new endpoints or features:

1. **Follow Rails conventions** - Use RESTful routes and standard controller actions
2. **Add appropriate tests** - Include request specs for new endpoints
3. **Update documentation** - Add new endpoints to this CONTRIBUTING.md file
4. **Consider performance** - Be mindful of N+1 queries and inefficient operations
5. **Maintain backward compatibility** - Don't break existing endpoints
6. **Add monitoring** - Include appropriate logging and error handling

## License

This workload is provided as part of the Benchmarker project and follows the same license terms.
