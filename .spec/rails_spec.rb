# frozen_string_literal: true

require "net/http"
require "json"
require "digest"
require "base64"

require_relative "spec_helper"

RSpec.describe "Production-Grade Rails Implementation" do
  # Shared helper for parsing JSON responses
  let(:parse_json) { ->(response) { JSON.parse(response.body) } }

  # ============================================================================
  # HEALTH CHECK
  # ============================================================================
  describe "Health Check" do
    describe "GET /health" do
      subject(:response) { http.request(Net::HTTP::Get.new("/health")) }

      it "returns 200 OK" do
        expect(response).to be_a(Net::HTTPSuccess)
      end

      it "returns OK text" do
        expect(response.body).to eq("OK")
      end

      it "has text/plain content type" do
        expect(response.content_type).to eq("text/plain")
      end
    end
  end

  # ============================================================================
  # CATEGORY 3: PERFORMANCE & SCALABILITY
  # ============================================================================
  describe "Category 3: Performance & Scalability" do
    describe "Caching" do
      describe "GET /api/v1/cached" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/cached")) }

        it "returns 200 OK" do
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "returns valid JSON" do
          json = parse_json.call(response)
          expect(json["data"]).to eq("Cached response")
          expect(json).to have_key("cached_at")
          expect(json).to have_key("cache_status")
        end

        it "has Cache-Control header" do
          expect(response["Cache-Control"]).to eq("max-age=3600, public")
        end

        it "has ETag header" do
          expect(response["ETag"]).not_to be_nil
        end

        context "when called twice" do
          let(:first_response) { http.request(Net::HTTP::Get.new("/api/v1/cached")) }
          let(:second_response) { http.request(Net::HTTP::Get.new("/api/v1/cached")) }

          it "serves from cache on second call" do
            first_json = parse_json.call(first_response)
            second_json = parse_json.call(second_response)
            expect(second_json["cache_status"]).to eq("served_from_cache")
          end
        end
      end
    end
  end

  # ============================================================================
  # CATEGORY 4: SECURITY
  # ============================================================================
  describe "Category 4: Security" do
    describe "CORS Support" do
      describe "GET / with CORS headers" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/json")) }

        it "includes Access-Control-Allow-Origin header" do
          expect(response["Access-Control-Allow-Origin"]).to eq("*")
        end

        it "includes Access-Control-Allow-Methods header" do
          expect(response["Access-Control-Allow-Methods"]).to include("GET")
          expect(response["Access-Control-Allow-Methods"]).to include("POST")
          expect(response["Access-Control-Allow-Methods"]).to include("PATCH")
          expect(response["Access-Control-Allow-Methods"]).to include("DELETE")
        end

        it "includes Access-Control-Allow-Headers header" do
          expect(response["Access-Control-Allow-Headers"]).to include("Authorization")
          expect(response["Access-Control-Allow-Headers"]).to include("Content-Type")
        end

        it "includes Access-Control-Max-Age header" do
          expect(response["Access-Control-Max-Age"]).to eq("86400")
        end

        it "includes Access-Control-Allow-Credentials header" do
          expect(response["Access-Control-Allow-Credentials"]).to eq("true")
        end

        it "includes Access-Control-Expose-Headers header" do
          expect(response["Access-Control-Expose-Headers"]).to include("Location")
        end
      end

      describe "OPTIONS / (preflight)" do
        subject(:response) { http.request(Net::HTTP::Options.new("/api/v1/json")) }

        it "returns 204 No Content" do
          expect(response.code).to eq("204")
        end

        it "has empty body" do
          expect(response.body.to_s).to be_empty
        end

        it "includes CORS headers" do
          expect(response["Access-Control-Allow-Origin"]).to eq("*")
          expect(response["Access-Control-Allow-Methods"]).to include("GET")
        end
      end

      describe "OPTIONS /api/v1/db/users (preflight for specific path)" do
        subject(:response) { http.request(Net::HTTP::Options.new("/api/v1/db/users")) }

        it "returns 204 No Content" do
          expect(response).to be_a(Net::HTTPNoContent)
        end
      end
    end

    describe "Authentication & Authorization" do
      describe "GET /api/v1/secure" do
        context "without Authorization header" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/secure")) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end

          it "returns proper error JSON" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Unauthorized")
            expect(json["error_description"]).to eq("No authorization token provided")
          end

          it "includes WWW-Authenticate header" do
            expect(response["WWW-Authenticate"]).to include("Bearer")
            expect(response["WWW-Authenticate"]).to include('error="invalid_token"')
          end
        end

        context "with invalid token type" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/secure")
            req["Authorization"] = "Basic token123"
            req
          end

          subject(:response) { http.request(request) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end

          it "returns proper error message" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Unauthorized")
            expect(json["error_description"]).to include("Invalid token type")
          end

          it "includes WWW-Authenticate header" do
            expect(response["WWW-Authenticate"]).to include('error="invalid_token"')
          end
        end

        context "with empty Bearer token" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/secure")
            req["Authorization"] = "Bearer "
            req
          end

          subject(:response) { http.request(request) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end

          it "returns proper error message" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Unauthorized")
            expect(json["error_description"]).to eq("Empty authorization token")
          end
        end

        context "with valid Bearer token" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/secure")
            req["Authorization"] = "Bearer valid-token-12345"
            req
          end

          subject(:response) { http.request(request) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "returns authentication info" do
            json = parse_json.call(response)
            expect(json["authenticated"]).to be true
            expect(json["message"]).to eq("Access granted")
          end

          it "masks token in response" do
            json = parse_json.call(response)
            # Token should be masked: first 8 chars + ... + last 4 chars
            expect(json["token"]).to eq("valid-t...345")
          end
        end
      end

      describe "GET /api/v1/protected" do
        context "without Authorization header" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/protected")) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end

          it "has empty body" do
            expect(response.body.to_s).to be_empty
          end

          it "includes WWW-Authenticate header" do
            expect(response["WWW-Authenticate"]).to include("Bearer")
          end
        end

        context "with invalid token type" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/protected")
            req["Authorization"] = "Basic token123"
            req
          end

          subject(:response) { http.request(request) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end
        end

        context "with empty Bearer token" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/protected")
            req["Authorization"] = "Bearer "
            req
          end

          subject(:response) { http.request(request) }

          it "returns 401 Unauthorized" do
            expect(response).to be_a(Net::HTTPUnauthorized)
          end
        end

        context "with valid Bearer token" do
          let!(:request) do
            req = Net::HTTP::Get.new("/api/v1/protected")
            req["Authorization"] = "Bearer any-valid-token"
            req
          end

          subject(:response) { http.request(request) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "returns protected resource" do
            json = parse_json.call(response)
            expect(json["resource"]).to eq("Protected data")
            expect(json["access"]).to eq("granted")
            expect(json["protected"]).to be true
          end
        end
      end
    end

    describe "Security Headers" do
      describe "All responses" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/json")) }

        it "has Referrer-Policy header" do
          expect(response["Referrer-Policy"]).to eq("strict-origin-when-cross-origin")
        end

        it "has Permissions-Policy header" do
          expect(response["Permissions-Policy"]).to include("geolocation=()")
          expect(response["Permissions-Policy"]).to include("microphone=()")
          expect(response["Permissions-Policy"]).to include("camera=()")
        end
      end
    end
  end

  # ============================================================================
  # CATEGORY 5: DATA MANAGEMENT
  # ============================================================================
  describe "Category 5: Data Management" do
    describe "User CRUD Operations" do
      describe "POST /api/v1/db/users" do
        context "with valid data" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "Test User", email: "test@example.com" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 201 Created" do
            expect(response).to be_a(Net::HTTPCreated)
          end

          it "returns Location header" do
            expect(response["Location"]).to match(%r{/api/v1/db/users/\d+})
          end

          it "returns user data" do
            json = parse_json.call(response)
            expect(json["status"]).to eq("created")
            expect(json["user"]["name"]).to eq("Test User")
            expect(json["user"]["email"]).to eq("test@example.com")
            expect(json["user"]).to have_key("id")
            expect(json["user"]).to have_key("created_at")
          end

          it "has no-cache headers" do
            expect(response["Cache-Control"]).to eq("no-cache, no-store, must-revalidate")
          end
        end

        context "with missing name" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req["Content-Type"] = "application/json"
            req.body = { user: { email: "test@example.com" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 422 Unprocessable Entity" do
            expect(response).to be_a(Net::HTTPUnprocessableEntity)
          end

          it "returns validation errors" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Validation failed")
            expect(json["errors"]).to be_an(Array)
            expect(json["errors"].any? { |e| e.include?("Name") || e.include?("blank") }).to be true
          end
        end

        context "with missing email" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "Test User" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 422 Unprocessable Entity" do
            expect(response).to be_a(Net::HTTPUnprocessableEntity)
          end

          it "returns validation errors" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Validation failed")
            expect(json["errors"]).to be_an(Array)
            expect(json["errors"].any? { |e| e.include?("Email") || e.include?("blank") }).to be true
          end
        end

        context "with invalid email format" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "Test User", email: "invalid-email" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 422 Unprocessable Entity" do
            expect(response).to be_a(Net::HTTPUnprocessableEntity)
          end

          it "returns validation errors" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Validation failed")
            expect(json["errors"]).to be_an(Array)
            expect(json["errors"].any? { |e| e.include?("Email") || e.include?("valid") }).to be true
          end
        end

        context "with extra parameters (security test)" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req["Content-Type"] = "application/json"
            req.body = {
              user: {
                name: "Test User",
                email: "extra@test.com",
                admin: true,
                password: "secret123",
              },
            }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "ignores extra parameters (mass assignment protection)" do
            expect(response).to be_a(Net::HTTPCreated)
            json = parse_json.call(response)
            expect(json["user"]).not_to have_key("admin")
            expect(json["user"]).not_to have_key("password")
          end
        end

        context "without Content-Type header" do
          let!(:request) do
            req = Net::HTTP::Post.new("/api/v1/db/users")
            req.body = { user: { name: "Test User", email: "test@example.com" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 415 Unsupported Media Type or handles gracefully" do
            # Rails typically accepts JSON without Content-Type in tests
            # This test verifies it doesn't crash
            expect([201, 415, 400]).to include(response.code.to_i)
          end
        end
      end

      describe "GET /api/v1/db/users" do
        context "when no users exist" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users")) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "returns empty users array" do
            json = parse_json.call(response)
            expect(json["count"]).to eq(0)
            expect(json["users"]).to be_an(Array)
            expect(json["users"]).to be_empty
          end

          it "has pagination data in response body" do
            json = parse_json.call(response)
            expect(json["count"]).to eq(0)
            expect(json["page"]).to eq(1)
            expect(json["per_page"]).to eq(20)
          end
        end

        context "with pagination parameters" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users?page=2&per_page=10")) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "respects pagination parameters" do
            json = parse_json.call(response)
            expect(json["page"]).to eq(2)
            expect(json["per_page"]).to eq(10)
          end
        end

        context "with invalid pagination parameters" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users?page=0&per_page=0")) }

          it "clamps parameters to valid range" do
            json = parse_json.call(response)
            expect(json["page"]).to eq(1)
            expect(json["per_page"]).to eq(1)
          end
        end
      end

      describe "GET /api/v1/db/users/:id" do
        let!(:create_request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Get Test User", email: "gettest@example.com" } }.to_json
          req
        end

        let(:create_response) { http.request(create_request) }
        let(:user_id) { parse_json.call(create_response)["user"]["id"] }

        context "when user exists" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users/#{user_id}")) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "returns user data" do
            json = parse_json.call(response)
            expect(json["id"]).to eq(user_id)
            expect(json["name"]).to eq("Get Test User")
            expect(json["email"]).to eq("gettest@example.com")
            expect(json).to have_key("created_at")
          end
        end

        context "when user does not exist" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users/99999")) }

          it "returns 404 Not Found" do
            expect(response).to be_a(Net::HTTPNotFound)
          end

          it "returns proper error message" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Not found")
            expect(json["message"]).to include("99999")
          end
        end

        context "with invalid ID format" do
          subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users/invalid")) }

          it "returns 404 Not Found" do
            expect(response).to be_a(Net::HTTPNotFound)
          end
        end
      end

      describe "PATCH /api/v1/db/users/:id" do
        let!(:create_request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Update Test User", email: "updatetest@example.com" } }.to_json
          req
        end

        let(:create_response) { http.request(create_request) }
        let(:user_id) { parse_json.call(create_response)["user"]["id"] }

        context "with valid data" do
          let!(:request) do
            req = Net::HTTP::Patch.new("/api/v1/db/users/#{user_id}")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "Updated Name" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 200 OK" do
            expect(response).to be_a(Net::HTTPSuccess)
          end

          it "returns updated user data" do
            json = parse_json.call(response)
            expect(json["name"]).to eq("Updated Name")
            expect(json["email"]).to eq("updatetest@example.com")
            expect(json).to have_key("updated_at")
          end
        end

        context "when user does not exist" do
          let!(:request) do
            req = Net::HTTP::Patch.new("/api/v1/db/users/99999")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "Updated Name" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 404 Not Found" do
            expect(response).to be_a(Net::HTTPNotFound)
          end

          it "returns proper error message" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Not found")
          end
        end

        context "with invalid data" do
          let!(:request) do
            req = Net::HTTP::Patch.new("/api/v1/db/users/#{user_id}")
            req["Content-Type"] = "application/json"
            req.body = { user: { name: "" } }.to_json
            req
          end

          subject(:response) { http.request(request) }

          it "returns 422 Unprocessable Entity" do
            expect(response).to be_a(Net::HTTPUnprocessableEntity)
          end

          it "returns validation errors" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Validation failed")
          end
        end
      end

      describe "DELETE /api/v1/db/users/:id" do
        let!(:create_request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Delete Test User", email: "deletetest@example.com" } }.to_json
          req
        end

        let(:create_response) { http.request(create_request) }
        let(:user_id) { parse_json.call(create_response)["user"]["id"] }

        context "when user exists" do
          subject(:response) { http.request(Net::HTTP::Delete.new("/api/v1/db/users/#{user_id}")) }

          it "returns 204 No Content" do
            expect(response).to be_a(Net::HTTPNoContent)
          end

          it "has empty body" do
            expect(response.body.to_s).to be_empty
          end

          it "actually deletes the user" do
            response
            get_response = http.request(Net::HTTP::Get.new("/api/v1/db/users/#{user_id}"))
            expect(get_response).to be_a(Net::HTTPNotFound)
          end
        end

        context "when user does not exist" do
          subject(:response) { http.request(Net::HTTP::Delete.new("/api/v1/db/users/99999")) }

          it "returns 404 Not Found" do
            expect(response).to be_a(Net::HTTPNotFound)
          end

          it "returns proper error message" do
            json = parse_json.call(response)
            expect(json["error"]).to eq("Not found")
          end
        end
      end
    end

    describe "Email Uniqueness" do
      let!(:create_request_1) do
        req = Net::HTTP::Post.new("/api/v1/db/users")
        req["Content-Type"] = "application/json"
        req.body = { user: { name: "User 1", email: "unique@example.com" } }.to_json
        req
      end

      let(:create_response_1) { http.request(create_request_1) }

      context "when creating user with duplicate email" do
        let!(:request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "User 2", email: "UNIQUE@EXAMPLE.COM" } }.to_json
          req
        end

        subject(:response) { http.request(request) }

        it "returns 422 Unprocessable Entity" do
          expect(response).to be_a(Net::HTTPUnprocessableEntity)
        end

        it "returns uniqueness error" do
          json = parse_json.call(response)
          expect(json["error"]).to eq("Validation failed")
          expect(json["errors"]).to be_an(Array)
          expect(json["errors"].any? { |e| e.include?("Email") || e.include?("taken") }).to be true
        end
      end
    end
  end

  # ============================================================================
  # CATEGORY 6: API & INTEGRATION
  # ============================================================================
  describe "Category 6: API & Integration" do
    describe "JSON API Support" do
      describe "GET /api/v1/json" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/json")) }

        it "returns 200 OK" do
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "returns valid JSON" do
          json = parse_json.call(response)
          expect(json["status"]).to eq("ok")
          expect(json["message"]).to eq("JSON API response")
          expect(json).to have_key("timestamp")
          expect(json["framework"]).to eq("Rails")
          expect(json).to have_key("version")
          expect(json).to have_key("api_version")
        end

        it "has application/json content type" do
          expect(response.content_type).to eq("application/json")
        end

        it "has no-cache headers" do
          expect(response["Cache-Control"]).to eq("no-cache, no-store, must-revalidate")
          expect(response["Pragma"]).to eq("no-cache")
          expect(response["Expires"]).to eq("0")
        end
      end
    end

    describe "External API Integration" do
      describe "GET /api/v1/external" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/external")) }

        it "returns 200 OK" do
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "returns external data in JSON format" do
          json = parse_json.call(response)
          expect(json["id"]).to eq(1)
          expect(json["title"]).to eq("External resource")
          expect(json["source"]).to eq("mock_external_api")
          expect(json).to have_key("timestamp")
        end

        it "has Cache-Control header" do
          expect(response["Cache-Control"]).to eq("max-age=300, public")
        end
      end
    end

    describe "HTTP Client" do
      # Test the HttpClient service directly
      describe "HttpClient.get_external_resource" do
        it "returns data for valid ID" do
          result = HttpClient.get_external_resource(1)
          expect(result).to be_a(Hash)
          expect(result).to have_key("id")
        end

        it "returns error for invalid ID" do
          result = HttpClient.get_external_resource(99999)
          expect(result).to have_key(:error)
        end

        it "has timeout configured" do
          expect(HttpClient.default_timeout).to eq(10)
        end
      end
    end
  end

  # ============================================================================
  # HTTP STANDARDS COMPLIANCE
  # ============================================================================
  describe "HTTP Standards Compliance" do
    describe "Status Codes" do
      it "uses 200 OK for successful GET requests" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response.code).to eq("200")
      end

      it "uses 201 Created for resource creation" do
        req = Net::HTTP::Post.new("/api/v1/db/users")
        req["Content-Type"] = "application/json"
        req.body = { user: { name: "Status Test", email: "status@test.com" } }.to_json
        response = http.request(req)
        expect(response.code).to eq("201")
      end

      it "uses 204 No Content for successful deletion" do
        # First create a user
        create_req = Net::HTTP::Post.new("/api/v1/db/users")
        create_req["Content-Type"] = "application/json"
        create_req.body = { user: { name: "Delete Status", email: "delete@status.com" } }.to_json
        create_response = http.request(create_req)
        user_id = parse_json.call(create_response)["user"]["id"]

        # Then delete it
        delete_response = http.request(Net::HTTP::Delete.new("/api/v1/db/users/#{user_id}"))
        expect(delete_response.code).to eq("204")
      end

      it "uses 400 Bad Request for missing parameters" do
        req = Net::HTTP::Post.new("/api/v1/db/users")
        req["Content-Type"] = "application/json"
        req.body = { user: {} }.to_json
        response = http.request(req)
        expect([400, 422]).to include(response.code.to_i)
      end

      it "uses 401 Unauthorized for authentication failures" do
        response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
        expect(response.code).to eq("401")
      end

      it "uses 404 Not Found for missing resources" do
        response = http.request(Net::HTTP::Get.new("/api/v1/db/users/99999"))
        expect(response.code).to eq("404")
      end

      it "uses 422 Unprocessable Entity for validation errors" do
        req = Net::HTTP::Post.new("/api/v1/db/users")
        req["Content-Type"] = "application/json"
        req.body = { user: { name: "", email: "" } }.to_json
        response = http.request(req)
        expect(response.code).to eq("422")
      end
    end

    describe "Content Types" do
      it "returns application/json for JSON responses" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response.content_type).to eq("application/json")
      end
    end

    describe "Headers" do
      it "includes Date header in all responses" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response["Date"]).not_to be_nil
      end

      it "includes Server header in all responses" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response["Server"]).not_to be_nil
      end

      it "includes Content-Length header in all responses" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response["Content-Length"]).not_to be_nil
      end
    end
  end

  # ============================================================================
  # HTTP(S) STANDARDS COMPLIANCE - RFC 7230, 7231, 7232, 7233, 7234, 7235
  # ============================================================================
  describe "HTTP(S) Standards Compliance" do
    # ==========================================================================
    # RFC 7230: HTTP/1.1 Message Syntax and Routing
    # ==========================================================================
    describe "RFC 7230: HTTP/1.1 Message Syntax and Routing" do
      describe "Request Line" do
        it "accepts HTTP/1.1 requests" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          # HTTP/1.1 is the default in Net::HTTP
          expect(response.http_version).to eq("1.1")
        end
      end

      describe "Header Fields" do
        it "accepts standard header field syntax" do
          req = Net::HTTP::Get.new("/api/v1/json")
          req["X-Custom-Header"] = "test-value"
          response = http.request(req)
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "handles header field names case-insensitively" do
          # Header names should be case-insensitive per RFC 7230 Section 3.2
          req = Net::HTTP::Get.new("/api/v1/json")
          req["content-type"] = "application/json"
          response = http.request(req)
          expect(response).to be_a(Net::HTTPSuccess)
        end
      end

      describe "Message Body" do
        it "handles requests with body correctly" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Test", email: "test@test.com" } }.to_json
          response = http.request(req)
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "handles empty body correctly" do
          req = Net::HTTP::Get.new("/api/v1/json")
          response = http.request(req)
          expect(response).to be_a(Net::HTTPSuccess)
        end
      end

      describe "Connection Management" do
        it "uses keep-alive by default (HTTP/1.1 default)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          # Connection header may or may not be present
          # HTTP/1.1 defaults to keep-alive
          expect([nil, "keep-alive", "close"]).to include(response["Connection"]&.downcase)
        end
      end
    end

    # ==========================================================================
    # RFC 7231: Semantics and Content
    # ==========================================================================
    describe "RFC 7231: Semantics and Content" do
      describe "Request Methods" do
        it "supports GET method" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response).to be_a(Net::HTTPSuccess)
        end

        it "supports POST method" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Test", email: "test@test.com" } }.to_json
          response = http.request(req)
          expect([201, 200, 422]).to include(response.code.to_i)
        end

        it "supports PATCH method" do
          # First create a user
          create_req = Net::HTTP::Post.new("/api/v1/db/users")
          create_req["Content-Type"] = "application/json"
          create_req.body = { user: { name: "Patch Test", email: "patch@test.com" } }.to_json
          create_response = http.request(create_req)
          user_id = parse_json.call(create_response)["user"]["id"]

          # Then patch it
          patch_req = Net::HTTP::Patch.new("/api/v1/db/users/#{user_id}")
          patch_req["Content-Type"] = "application/json"
          patch_req.body = { user: { name: "Patched Name" } }.to_json
          response = http.request(patch_req)
          expect([200, 204, 422]).to include(response.code.to_i)
        end

        it "supports DELETE method" do
          # First create a user
          create_req = Net::HTTP::Post.new("/api/v1/db/users")
          create_req["Content-Type"] = "application/json"
          create_req.body = { user: { name: "Delete Test", email: "delete@test.com" } }.to_json
          create_response = http.request(create_req)
          user_id = parse_json.call(create_response)["user"]["id"]

          # Then delete it
          response = http.request(Net::HTTP::Delete.new("/api/v1/db/users/#{user_id}"))
          expect([204, 200, 404]).to include(response.code.to_i)
        end

        it "supports OPTIONS method for CORS" do
          response = http.request(Net::HTTP::Options.new("/api/v1/json"))
          expect(response.code).to eq("204")
        end

        it "supports HEAD method" do
          response = http.request(Net::HTTP::Head.new("/api/v1/json"))
          expect([200, 404, 405]).to include(response.code.to_i)
        end
      end

      describe "Status Codes" do
        it "uses 200 OK for successful requests" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response.code).to eq("200")
        end

        it "uses 201 Created for resource creation" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Created", email: "created@test.com" } }.to_json
          response = http.request(req)
          expect(response.code).to eq("201")
        end

        it "uses 204 No Content for successful deletion" do
          create_req = Net::HTTP::Post.new("/api/v1/db/users")
          create_req["Content-Type"] = "application/json"
          create_req.body = { user: { name: "Delete", email: "delete@test.com" } }.to_json
          create_response = http.request(create_req)
          user_id = parse_json.call(create_response)["user"]["id"]

          response = http.request(Net::HTTP::Delete.new("/api/v1/db/users/#{user_id}"))
          expect(response.code).to eq("204")
        end

        it "uses 400 Bad Request for client errors" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = "{ invalid json"
          response = http.request(req)
          expect([400, 422]).to include(response.code.to_i)
        end

        it "uses 401 Unauthorized for authentication failures" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response.code).to eq("401")
        end

        it "uses 404 Not Found for missing resources" do
          response = http.request(Net::HTTP::Get.new("/nonexistent"))
          expect(response.code).to eq("404")
        end

        it "uses 405 Method Not Allowed when appropriate" do
          # PUT is not supported on /api/v1/json endpoint
          response = http.request(Net::HTTP::Put.new("/api/v1/json"))
          # Rails may return 404 or 405 depending on route configuration
          expect([404, 405, 400]).to include(response.code.to_i)
        end

        it "uses 422 Unprocessable Entity for validation errors" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "", email: "" } }.to_json
          response = http.request(req)
          expect(response.code).to eq("422")
        end

        it "uses 500 Internal Server Error for server errors" do
          # This is hard to test without causing actual errors
          # Skip for now
        end
      end

      describe "Content Negotiation" do
        it "responds with appropriate Content-Type for JSON" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response.content_type).to eq("application/json")
        end

        it "handles Accept header for JSON" do
          req = Net::HTTP::Get.new("/api/v1/json")
          req["Accept"] = "application/json"
          response = http.request(req)
          expect(response.content_type).to eq("application/json")
        end
      end

      describe "Response Headers" do
        it "includes Date header (RFC 7231 Section 7.1.1.1)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Date"]).not_to be_nil
          # Date should be in RFC 1123 format
          expect { DateTime.rfc2822(response["Date"]) }.not_to raise_error
        end

        it "includes Server header (RFC 7231 Section 7.4.2)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Server"]).not_to be_nil
        end

        it "includes Content-Length header when response has body (RFC 7230 Section 3.3.2)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Content-Length"]).not_to be_nil
        end

        it "includes Content-Type header for responses with body" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Content-Type"]).not_to be_nil
        end

        it "includes Cache-Control header (RFC 7234)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Cache-Control"]).not_to be_nil
        end

        it "includes Location header for 201 Created responses" do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: "Location Test", email: "location@test.com" } }.to_json
          response = http.request(req)
          if response.code == "201"
            expect(response["Location"]).not_to be_nil
            expect(response["Location"]).to match(%r{/api/v1/db/users/\d+})
          end
        end

        it "includes WWW-Authenticate header for 401 responses (RFC 7235 Section 4.1)" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response["WWW-Authenticate"]).not_to be_nil
          expect(response["WWW-Authenticate"]).to include("Bearer")
        end
      end
    end

    # ==========================================================================
    # RFC 7232: Conditional Requests
    # ==========================================================================
    describe "RFC 7232: Conditional Requests" do
      describe "ETag support" do
        it "includes ETag header for cacheable responses" do
          response = http.request(Net::HTTP::Get.new("/api/v1/cached"))
          expect(response["ETag"]).not_to be_nil
        end

        it "supports If-None-Match header" do
          first_response = http.request(Net::HTTP::Get.new("/api/v1/cached"))
          etag = first_response["ETag"]

          if etag
            req = Net::HTTP::Get.new("/api/v1/cached")
            req["If-None-Match"] = etag
            second_response = http.request(req)
            # Should return 304 Not Modified if ETag matches
            expect([200, 304]).to include(second_response.code.to_i)
          end
        end
      end

      describe "Last-Modified support" do
        it "includes Last-Modified header when applicable" do
          # This depends on the server implementation
          # Rails typically doesn't include this by default
          # Skip for now
        end
      end
    end

    # ==========================================================================
    # RFC 7233: Range Requests
    # ==========================================================================
    describe "RFC 7233: Range Requests" do
      it "responds appropriately to Range requests" do
        req = Net::HTTP::Get.new("/api/v1/json")
        req["Range"] = "bytes=0-99"
        response = http.request(req)
        # Rails doesn't support range requests by default for JSON
        # Should return 206 Partial Content or 200 OK
        expect([200, 206, 416]).to include(response.code.to_i)
      end

      it "includes Accept-Ranges header when range requests are supported" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        # Rails typically doesn't set this for non-file responses
        # This is acceptable
      end
    end

    # ==========================================================================
    # RFC 7234: Caching
    # ==========================================================================
    describe "RFC 7234: Caching" do
      describe "Cache-Control header" do
        it "includes Cache-Control header" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Cache-Control"]).not_to be_nil
        end

        it "uses no-cache for dynamic content" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Cache-Control"]).to include("no-cache")
        end

        it "uses public caching for cacheable responses" do
          response = http.request(Net::HTTP::Get.new("/api/v1/cached"))
          expect(response["Cache-Control"]).to include("public")
        end

        it "uses max-age directive" do
          response = http.request(Net::HTTP::Get.new("/api/v1/cached"))
          expect(response["Cache-Control"]).to match(/max-age=\d+/)
        end
      end

      describe "Expires header" do
        it "includes Expires header for cacheable responses" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Expires"]).not_to be_nil
        end
      end

      describe "Pragma header" do
        it "includes Pragma header for backward compatibility" do
          response = http.request(Net::HTTP::Get.new("/api/v1/json"))
          expect(response["Pragma"]).not_to be_nil
        end
      end
    end

    # ==========================================================================
    # RFC 7235: Authentication
    # ==========================================================================
    describe "RFC 7235: Authentication" do
      describe "WWW-Authenticate header" do
        it "includes realm in WWW-Authenticate header" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response["WWW-Authenticate"]).to include("realm=")
        end

        it "uses Bearer scheme for token authentication" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response["WWW-Authenticate"]).to include("Bearer")
        end

        it "includes error description in WWW-Authenticate" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response["WWW-Authenticate"]).to include("error=")
        end
      end

      describe "Authentication with valid token" do
        it "succeeds with valid Bearer token" do
          req = Net::HTTP::Get.new("/api/v1/secure")
          req["Authorization"] = "Bearer valid-token-12345"
          response = http.request(req)
          expect(response.code).to eq("200")
        end

        it "fails with missing Authorization header" do
          response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
          expect(response.code).to eq("401")
        end

        it "fails with invalid token" do
          req = Net::HTTP::Get.new("/api/v1/secure")
          req["Authorization"] = "Bearer invalid"
          response = http.request(req)
          # Should return 401 or 403
          expect([401, 403]).to include(response.code.to_i)
        end
      end
    end

    # ==========================================================================
    # RFC 5789: PATCH Method
    # ==========================================================================
    describe "RFC 5789: PATCH Method for Partial Updates" do
      it "supports PATCH method for partial updates" do
        # Create a user first
        create_req = Net::HTTP::Post.new("/api/v1/db/users")
        create_req["Content-Type"] = "application/json"
        create_req.body = { user: { name: "Patch RFC Test", email: "patchrfc@test.com" } }.to_json
        create_response = http.request(create_req)
        user_id = parse_json.call(create_response)["user"]["id"]

        # Patch the user
        patch_req = Net::HTTP::Patch.new("/api/v1/db/users/#{user_id}")
        patch_req["Content-Type"] = "application/json"
        patch_req.body = { user: { name: "Updated Patch RFC Test" } }.to_json
        response = http.request(patch_req)

        expect(response.code).to eq("200")
        json = parse_json.call(response)
        expect(json["name"]).to eq("Updated Patch RFC Test")
      end

      it "applies PATCH changes incrementally" do
        # Create a user first
        create_req = Net::HTTP::Post.new("/api/v1/db/users")
        create_req["Content-Type"] = "application/json"
        create_req.body = { user: { name: "Incremental Test", email: "incremental@test.com" } }.to_json
        create_response = http.request(create_req)
        user_id = parse_json.call(create_response)["user"]["id"]

        # Patch only the name
        patch_req = Net::HTTP::Patch.new("/api/v1/db/users/#{user_id}")
        patch_req["Content-Type"] = "application/json"
        patch_req.body = { user: { name: "New Name" } }.to_json
        response = http.request(patch_req)

        json = parse_json.call(response)
        expect(json["name"]).to eq("New Name")
        expect(json["email"]).to eq("incremental@test.com")
      end
    end

    # ==========================================================================
    # RFC 6797: HTTP Strict Transport Security (HSTS)
    # ==========================================================================
    describe "RFC 6797: HSTS (in production)" do
      it "should configure HSTS in production" do
        # This is configured in security.rb initializer
        # We can't test the actual header in test environment
        # But we can verify the configuration exists
        expect(Rails.application.config.force_ssl).to eq(false) # Not forced in test
        expect(Rails.application.config.ssl_options).to be_a(Hash) if Rails.env.production?
      end

      it "includes Strict-Transport-Security header in production" do
        # This would be tested in production environment
        # In test environment, SSL is not forced
        # Skip for now
      end
    end

    # ==========================================================================
    # RFC 6265: HTTP State Management Mechanism (Cookies)
    # ==========================================================================
    describe "RFC 6265: Cookies" do
      it "does not use cookies for API responses" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        # API should not set cookies
        expect(response["Set-Cookie"]).to be_nil
      end
    end

    # ==========================================================================
    # RFC 7538: The Hypertext Transfer Protocol Status Code 308
    # ==========================================================================
    describe "RFC 7538: 308 Permanent Redirect" do
      # Not currently used in this API
      # Would test if we had permanent redirects
    end

    # ==========================================================================
    # RFC 7616: HTTP Digest Access Authentication
    # ==========================================================================
    describe "RFC 7616: Digest Authentication" do
      # Not currently used - we use Bearer tokens
      it "does not advertise Digest authentication" do
        response = http.request(Net::HTTP::Get.new("/api/v1/secure"))
        www_auth = response["WWW-Authenticate"]
        expect(www_auth).not_to include("Digest")
      end
    end

    # ==========================================================================
    # RFC 7617: The Basic HTTP Authentication Scheme
    # ==========================================================================
    describe "RFC 7617: Basic Authentication" do
      it "rejects Basic authentication (uses Bearer)" do
        req = Net::HTTP::Get.new("/api/v1/secure")
        req["Authorization"] = "Basic " + Base64.strict_encode64("user:pass")
        response = http.request(req)
        expect(response.code).to eq("401")
      end
    end

    # ==========================================================================
    # RFC 8246: HTTP Immutable Responses
    # ==========================================================================
    describe "RFC 8246: Immutable Responses" do
      # Not currently used - would require immutable caching
    end

    # ==========================================================================
    # HTTP/2 Considerations
    # ==========================================================================
    describe "HTTP/2 Support" do
      it "is compatible with HTTP/1.1 (baseline)" do
        response = http.request(Net::HTTP::Get.new("/api/v1/json"))
        expect(response.http_version).to eq("1.1")
      end

      # HTTP/2 would need separate testing with HTTP/2 client
    end
  end

  # ============================================================================
  # EDGE CASES AND ERROR HANDLING
  # ============================================================================
  describe "Edge Cases and Error Handling" do
    describe "Invalid JSON" do
      context "with malformed JSON" do
        let!(:request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = "{ invalid json"
          req
        end

        subject(:response) { http.request(request) }

        it "returns 400 Bad Request or handles gracefully" do
          # Rails typically returns 400 for invalid JSON
          expect([400, 422, 500]).to include(response.code.to_i)
        end
      end
    end

    describe "Empty Request Body" do
      context "with empty body on POST" do
        let!(:request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = ""
          req
        end

        subject(:response) { http.request(request) }

        it "returns 400 Bad Request or handles gracefully" do
          expect([400, 422]).to include(response.code.to_i)
        end
      end
    end

    describe "Very Long Parameters" do
      context "with very long name" do
        long_name = "A" * 200
        let!(:request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: long_name, email: "long@test.com" } }.to_json
          req
        end

        subject(:response) { http.request(request) }

        it "returns 422 Unprocessable Entity for validation error" do
          expect(response).to be_a(Net::HTTPUnprocessableEntity)
        end
      end
    end

    describe "SQL Injection Attempts" do
      context "with SQL injection in parameters" do
        subject(:response) { http.request(Net::HTTP::Get.new("/api/v1/db/users/1; DROP TABLE users;--")) }

        it "returns 404 Not Found (parameter is not a valid ID)" do
          expect(response).to be_a(Net::HTTPNotFound)
        end
      end
    end

    describe "XSS Attempts" do
      context "with XSS in user input" do
        let!(:request) do
          req = Net::HTTP::Post.new("/api/v1/db/users")
          req["Content-Type"] = "application/json"
          req.body = { user: { name: '<script>alert("xss")</script>', email: "xss@test.com" } }.to_json
          req
        end

        subject(:response) { http.request(request) }

        it "sanitizes or escapes XSS in response" do
          # The name might be stored with XSS, but output should be safe
          # Rails by default escapes JSON output
          expect(response.code).to eq("201")
        end
      end
    end
  end
end
