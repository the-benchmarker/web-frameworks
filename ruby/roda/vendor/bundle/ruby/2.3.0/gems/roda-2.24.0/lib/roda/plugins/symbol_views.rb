# frozen-string-literal: true

#
class Roda
  module RodaPlugins
    # The symbol_views plugin allows match blocks to return
    # symbols, and consider those symbols as views to use for the
    # response body.  So you can take code like:
    #
    #   r.root do
    #     view :index
    #   end
    #   r.is "foo" do
    #     view :foo
    #   end
    #
    # and DRY it up:
    #
    #   r.root do
    #     :index
    #   end
    #   r.is "foo" do
    #     :foo
    #   end
    module SymbolViews
      module RequestMethods
        private

        # If the block result is a symbol, consider the symbol a
        # template name and use the template view as the body.
        def block_result_body(result)
          if result.is_a?(Symbol)
            scope.view(result)
          else
            super
          end
        end
      end
    end

    register_plugin(:symbol_views, SymbolViews)
  end
end
