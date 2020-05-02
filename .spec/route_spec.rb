# frozen_string_literal: true

require "net/http"

require_relative "spec_helper"

RSpec.describe "routes" do
  let!(:http) { Net::HTTP.new(ip(ENV["FRAMEWORK"]), 3000) }

  context "GET : /" do
    subject(:response) { http.request(Net::HTTP::Get.new("/")) }

    it "returns successfully" do
      expect response.is_a? Net::HTTPSuccess
    end

    it "returns an empty string" do
      expect response.body == ""
    end
  end

  context "GET : /user/0" do
    subject(:response) { http.request(Net::HTTP::Get.new("/user/0")) }

    it "returns successfully" do
      expect response.is_a? Net::HTTPSuccess
    end

    it "returns the expected param" do
      expect response.body == "0"
    end
  end

  context "POST : /user" do
    subject(:response) { http.request(Net::HTTP::Post.new("/user")) }

    it "returns successfully" do
      expect response.is_a? Net::HTTPSuccess
    end

    it "returns an empty string" do
      expect response.body == ""
    end
  end
end
