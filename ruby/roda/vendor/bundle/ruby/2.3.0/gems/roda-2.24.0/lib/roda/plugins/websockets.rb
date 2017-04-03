# frozen-string-literal: true

require 'faye/websocket'

class Roda
  module RodaPlugins
    # The websocket plugin adds integration support for websockets.
    # Currently, only 'faye-websocket' is supported, so eventmachine
    # is required for websockets.  See the
    # {faye-websocket documentation}[https://github.com/faye/faye-websocket-ruby] 
    # for details on the faye-websocket API. Note that faye-websocket
    # is only supported on ruby 1.9.3+, so the websockets plugin only works
    # on ruby 1.9.3+.
    #
    # Here's a simplified example for a basic multi-user,
    # multi-room chat server, where a message from any user in a room
    # is sent to all other users in the same room, using a websocket
    # per room:
    #
    #   plugin :websockets, :adapter=>:thin, :ping=>45
    #
    #   MUTEX = Mutex.new
    #   ROOMS = {}
    #
    #   def sync
    #     MUTEX.synchronize{yield}
    #   end
    #
    #   route do |r|
    #     r.get "room", :d do |room_id|
    #       room = sync{ROOMS[room_id] ||= []}
    #
    #       r.websocket do |ws|
    #         # Routing block taken if request is a websocket request,
    #         # yields a Faye::WebSocket instance
    #
    #         ws.on(:message) do |event|
    #           sync{room.dup}.each{|user| user.send(event.data)}
    #         end
    #
    #         ws.on(:close) do |event|
    #           sync{room.delete(ws)}
    #           sync{room.dup}.each{|user| user.send("Someone left")}
    #         end
    #
    #         sync{room.dup}.each{|user| user.send("Someone joined")}
    #         sync{room.push(ws)}
    #       end
    #
    #       # If the request is not a websocket request, execution
    #       # continues, similar to how routing in general works.
    #       view 'room'
    #     end
    #   end
    module Websockets
      WebSocket = ::Faye::WebSocket
      OPTS = {}.freeze

      # Add default opions used for websockets.  These options are
      # passed to Faye:WebSocket.new, except that the following
      # options are handled separately.
      #
      # :adapter :: Calls Faye::WebSocket.load adapter with the given
      #             value, used to set the adapter to load, if Faye
      #             requires an adapter to work with the webserver.
      #             Possible options: :thin, :rainbows, :goliath
      #
      # See RequestMethods#websocket for additional supported options.
      def self.configure(app, opts=OPTS)
        opts = app.opts[:websockets_opts] = (app.opts[:websockets_opts] || {}).merge(opts || {})
        if adapter = opts.delete(:adapter)
          WebSocket.load_adapter(adapter.to_s)
        end
        opts.freeze
      end

      module RequestMethods
        # True if this request is a websocket request, false otherwise.
        def websocket?
          WebSocket.websocket?(env)
        end

        # If the request is a websocket request, yield a websocket to the
        # block, and return the appropriate rack response after the block
        # returns.  +opts+ is an options hash used when creating the
        # websocket, except the following options are handled specially:
        #
        # :protocols :: Set the protocols to accept, should be an array
        #               of strings.
        def websocket(opts=OPTS)
          if websocket?
            always do
              opts = Hash[roda_class.opts[:websockets_opts]].merge!(opts)
              ws = WebSocket.new(env, opts.delete(:protocols), opts)
              yield ws
              halt ws.rack_response
            end
          end
        end
      end
    end

    register_plugin(:websockets, Websockets)
  end
end
