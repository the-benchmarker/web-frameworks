Rails.application.routes.draw do
  # Root endpoint for health checks
  get "/" => "application#index"
  
  # Legacy endpoints for backward compatibility
  get "/user/:id" => "application#user"
  post "/user" => "application#register_user"
  
  # API v1 routes
  namespace :api do
    namespace :v1 do
      # User resources
      resources :users, only: [:index, :show, :create, :update, :destroy] do
        member do
          get :posts
          get :comments
        end
      end
      
      # Post resources
      resources :posts, only: [:index, :show, :create, :update, :destroy] do
        member do
          get :comments
        end
        collection do
          get :recent
        end
      end
      
      # Comment resources
      resources :comments, only: [:index, :show, :create, :update, :destroy]
      
      # External service endpoints
      namespace :external do
        get 'posts', to: 'external#posts'
        get 'posts/:id', to: 'external#post'
        post 'posts', to: 'external#create_post'
        get 'health', to: 'external#health'
        get 'weather', to: 'external#weather'
        get 'rates', to: 'external#exchange_rates'
      end
      
      # Cache endpoints
      namespace :cache do
        get 'health', to: 'cache#health'
        post 'set', to: 'cache#set'
        get 'get', to: 'cache#get'
        delete 'delete', to: 'cache#delete'
        post 'increment', to: 'cache#increment'
        get 'stats', to: 'cache#stats'
      end
      
      # Background job endpoints
      namespace :jobs do
        post 'process_data', to: 'jobs#process_data'
        post 'send_notification', to: 'jobs#send_notification'
        post 'sync_external', to: 'jobs#sync_external'
        get ':id/status', to: 'jobs#status'
        get 'stats', to: 'jobs#stats'
        post 'batch', to: 'jobs#batch'
      end
    end
  end
  
  # Catch-all route for 404
  match '*path', to: 'application#not_found', via: :all
end
