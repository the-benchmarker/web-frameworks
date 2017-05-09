defmodule Gettext.ExtractorAgent do
  @moduledoc false

  @name __MODULE__

  # :translations is a map where keys are Gettext backends and values
  # are maps. In these maps, keys are domains and values are maps of
  # translation_id => translation.
  # :backends is just a list of backends that call `use Gettext`.
  @initial_state %{
    translations: %{},
    backends: [],
    extracting?: false
  }

  def start_link do
    Agent.start_link(fn -> @initial_state end, name: @name)
  end

  def enable do
    Agent.update(@name, &put_in(&1.extracting?, true))
  end

  def disable do
    Agent.update(@name, &put_in(&1.extracting?, false))
  end

  def extracting? do
    Agent.get(@name, & &1.extracting?)
  end

  def add_translation(backend, domain, translation) do
    key = Gettext.PO.Translations.key(translation)

    Agent.cast @name, fn(state) ->
      # Initialize the given backend to an empty map if it wasn't there.
      state = update_in state.translations, &Map.put_new(&1, backend, %{})

      update_in state, [:translations, backend, domain], fn(translations) ->
        Map.update(translations || %{}, key, translation, &merge_translations(&1, translation))
      end
    end
  end

  def add_backend(backend) do
    Agent.cast @name, fn(state) ->
      update_in state.backends, &[backend | &1]
    end
  end

  def stop do
    Agent.stop @name
  end

  def pop_translations(backends) do
    Agent.get_and_update @name, fn state ->
      get_and_update_in state.translations, &Map.split(&1, backends)
    end
  end

  def pop_backends(app) do
    Agent.get_and_update @name, fn state ->
      get_and_update_in state.backends, fn backends ->
        Enum.partition(backends, & &1.__gettext__(:otp_app) == app)
      end
    end
  end

  defp merge_translations(t1, t2) do
    update_in t1.references, &(&1 ++ t2.references)
  end
end
