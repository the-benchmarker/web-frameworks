defmodule Server.Router do
  @moduledoc """
  Phoenix Router Module
  
  Defines application routes with production-grade best practices.
  Implements RESTful routing conventions and proper error handling.
  """
  
  use Phoenix.Router

  @compile :native
  @compile {:hipe, [:o3]}

  # ===========================================================================
  # API Routes
  # ===========================================================================

  # Health check and status endpoint
  get "/", Server.Controller, :index

  # User resource routes - RESTful conventions
  resources "/user", Server.Controller, only: [:show, :create]

  # ===========================================================================
  # Pipeline Configuration
  # ===========================================================================

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  # ===========================================================================
  # Route Scoping
  # ===========================================================================

  scope "/api", Server do
    pipe_through :api
    
    get "/", Server.Controller, :index
    resources "/user", Server.Controller, only: [:show, :create]
  end

  scope "/", Server do
    pipe_through :browser
    
    get "/", Server.Controller, :index
    resources "/user", Server.Controller, only: [:show, :create]
  end
end
