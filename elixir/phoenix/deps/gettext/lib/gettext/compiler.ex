defmodule Gettext.Compiler do
  @moduledoc false

  alias Gettext.PO
  alias Gettext.PO.Translation
  alias Gettext.PO.PluralTranslation
  alias Gettext.Interpolation

  require Logger

  @default_priv "priv/gettext"
  @po_wildcard "*/LC_MESSAGES/*.po"

  @doc false
  defmacro __before_compile__(env) do
    # Opts given to "use Gettext" have higher precedence than options set
    # throught Mix.Config; :otp_app, however, is only supported in "use
    # Gettext" (because we need it to get the Mix config).

    compile_time_opts = Module.get_attribute(env.module, :gettext_opts)

    {otp_app, compile_time_opts} =
      case Keyword.pop(compile_time_opts, :otp_app) do
        {nil, opts} -> Keyword.fetch!(opts, :otp_app) # (using fetch!/2 to raise)
        {app, opts} -> {app, opts}
      end

    mix_config_opts = Application.get_env(otp_app, env.module, [])
    opts = Keyword.merge(mix_config_opts, compile_time_opts)

    priv = Keyword.get(opts, :priv, @default_priv)
    plural_forms = Keyword.get(opts, :plural_forms, Gettext.Plural)

    translations_dir = Application.app_dir(otp_app, priv)
    external_file = Path.join(".compile", priv) |> String.replace("/", "_")
    known_locales = known_locales(translations_dir)

    quote do
      @behaviour Gettext.Backend

      @doc false
      def __gettext__(:priv),          do: unquote(priv)
      def __gettext__(:otp_app),       do: unquote(otp_app)
      def __gettext__(:known_locales), do: unquote(known_locales)

      # The manifest lives in the root of the priv
      # directory that contains .po/.pot files.
      @external_resource unquote(Application.app_dir(otp_app, external_file))

      # This will be used when pluralizing in lngettext/6.
      @plural_forms unquote(plural_forms)

      if Gettext.Extractor.extracting? do
        Gettext.ExtractorAgent.add_backend(__MODULE__)
      end

      unquote(macros())
      unquote(signatures())
      unquote(compile_po_files(translations_dir))
      unquote(dynamic_clauses())
    end
  end

  defp macros do
    quote unquote: false do
      defmacro dgettext_noop(domain, msgid) do
        domain = Gettext.Compiler.expand_to_binary(domain, "domain", __MODULE__, __CALLER__)
        msgid = Gettext.Compiler.expand_to_binary(msgid, "msgid", __MODULE__, __CALLER__)

        if Gettext.Extractor.extracting? do
          Gettext.Extractor.extract(__CALLER__, __MODULE__, domain, msgid)
        end

        msgid
      end

      defmacro gettext_noop(msgid) do
        quote do
          unquote(__MODULE__).dgettext_noop("default", unquote(msgid))
        end
      end

      defmacro dngettext_noop(domain, msgid, msgid_plural) do
        domain = Gettext.Compiler.expand_to_binary(domain, "domain", __MODULE__, __CALLER__)
        msgid = Gettext.Compiler.expand_to_binary(msgid, "msgid", __MODULE__, __CALLER__)
        msgid_plural = Gettext.Compiler.expand_to_binary(msgid_plural, "msgid_plural", __MODULE__, __CALLER__)

        if Gettext.Extractor.extracting? do
          Gettext.Extractor.extract(__CALLER__, __MODULE__, domain, {msgid, msgid_plural})
        end

        {msgid, msgid_plural}
      end

      defmacro ngettext_noop(msgid, msgid_plural) do
        quote do
          unquote(__MODULE__).dngettext_noop("default", unquote(msgid), unquote(msgid_plural))
        end
      end

      defmacro dgettext(domain, msgid, bindings \\ Macro.escape(%{})) do
        quote do
          msgid = unquote(__MODULE__).dgettext_noop(unquote(domain), unquote(msgid))
          Gettext.dgettext(unquote(__MODULE__), unquote(domain), msgid, unquote(bindings))
        end
      end

      defmacro gettext(msgid, bindings \\ Macro.escape(%{})) do
        quote do
          unquote(__MODULE__).dgettext("default", unquote(msgid), unquote(bindings))
        end
      end

      defmacro dngettext(domain, msgid, msgid_plural, n, bindings \\ Macro.escape(%{})) do
        quote do
          {msgid, msgid_plural} =
            unquote(__MODULE__).dngettext_noop(unquote(domain), unquote(msgid), unquote(msgid_plural))

          Gettext.dngettext(
            unquote(__MODULE__),
            unquote(domain),
            msgid,
            msgid_plural,
            unquote(n),
            unquote(bindings)
          )
        end
      end

      defmacro ngettext(msgid, msgid_plural, n, bindings \\ Macro.escape(%{})) do
        quote do
          unquote(__MODULE__).dngettext(
            "default",
            unquote(msgid),
            unquote(msgid_plural),
            unquote(n),
            unquote(bindings)
          )
        end
      end
    end
  end

  defp signatures do
    quote do
      def lgettext(locale, domain, msgid, bindings)
      def lngettext(locale, domain, msgid, msgid_plural, n, bindings)
    end
  end

  @doc """
  Returns the quoted code for the dynamic clauses of `lgettext/4` and
  `lngettext/6`.
  """
  @spec dynamic_clauses() :: Macro.t
  def dynamic_clauses do
    quote do
      def lgettext(locale, domain, msgid, bindings) do
        import Gettext.Interpolation, only: [to_interpolatable: 1, interpolate: 2]

        Gettext.Compiler.warn_if_domain_contains_slashes(domain)

        case interpolate(to_interpolatable(msgid), bindings) do
          {:ok, interpolated} -> {:default, interpolated}
          other -> other
        end
      end

      def lngettext(_locale, domain, msgid, msgid_plural, n, bindings) do
        import Gettext.Interpolation, only: [to_interpolatable: 1, interpolate: 2]

        Gettext.Compiler.warn_if_domain_contains_slashes(domain)
        string = (if n == 1, do: msgid, else: msgid_plural)
        bindings = Map.put(bindings, :count, n)

        case interpolate(to_interpolatable(string), bindings) do
          {:ok, interpolated} -> {:default, interpolated}
          other -> other
        end
      end
    end
  end

  @doc """
  Expands the given `msgid` in the given `env`, raising if it doesn't expand to
  a binary.

  This function doesn't just check that the expansion of `msgid` (via
  `Macro.expand/2`) is a binary; it also takes care of `{:<<>>, _, binaries}`
  ASTs (e.g., the `~s` sigil expands to such AST).
  """
  @spec expand_to_binary(binary, binary, module, Macro.Env.t) ::
    binary | no_return
  def expand_to_binary(term, what, gettext_module, env) when what in ~w(domain msgid msgid_plural) do
    raiser = fn ->
      raise ArgumentError, """
      *gettext macros expect translation keys (msgid and msgid_plural) and
      domains to expand to strings at compile-time, but the given #{what}
      doesn't.

      Dynamic translations should be avoided as they limit gettext's
      ability to extract translations from your source code. If you are
      sure you need dynamic lookup, you can use the functions in the Gettext
      module:

          string = "hello world"
          Gettext.gettext(#{inspect gettext_module}, string)
      """
    end

    case Macro.expand(term, env) do
      term when is_binary(term) ->
        term
      {:<<>>, _, pieces} ->
        # TODO: Remove this once Elixir will fix ~s to expand to just a binary when
        # here's no interpolation.
        if Enum.all?(pieces, &is_binary/1), do: Enum.join(pieces, ""), else: raiser.()
      _ ->
        raiser.()
    end
  end

  @doc """
  Logs a warning via `Logger.error/1` if `domain` contains slashes.

  This function is called by `lgettext` and `lngettext`. It could make sense to
  make this function raise an error since slashes in domains are not supported,
  but we decided not to do so and to only emit a warning since the expected
  behaviour for Gettext functions/macros when the domain or translation is not
  known is to return the original string (msgid) and raising here would break
  that contract.
  """
  @spec warn_if_domain_contains_slashes(binary) :: :ok
  def warn_if_domain_contains_slashes(domain) do
    if String.contains?(domain, "/") do
      Logger.error(["slashes in domains are not supported: ", inspect(domain)])
    end
    :ok
  end

  @doc """
  Compiles all the `.po` files in the given directory (`dir`) into `lgettext/4`
  and `lngettext/6` function clauses.
  """
  @spec compile_po_files(Path.t) :: Macro.t
  def compile_po_files(dir) do
    Enum.flat_map(po_files_in_dir(dir), &compile_po_file/1)
  end

  # Compiles a .po file into a list of lgettext/4 (for translations) and
  # lngettext/6 (for plural translations) clauses.
  defp compile_po_file(path) do
    {locale, domain} = locale_and_domain_from_path(path)
    %PO{translations: translations} = PO.parse_file!(path)
    Enum.map(translations, &compile_translation(locale, domain, &1))
  end

  defp locale_and_domain_from_path(path) do
    [file, "LC_MESSAGES", locale | _rest] = path |> Path.split |> Enum.reverse
    domain = Path.rootname(file, ".po")
    {locale, domain}
  end

  defp compile_translation(locale, domain, %Translation{} = t) do
    msgid  = IO.iodata_to_binary(t.msgid)
    msgstr = IO.iodata_to_binary(t.msgstr)

    # Only actually generate this function clause if the msgstr is not empty. If
    # it's empty, not generating this clause (by returning `nil` from this `if`)
    # means that the dynamic clause will be executed, returning `{:default,
    # msgid}` (with interpolation and so on).
    if msgstr != "" do
      quote do
        def lgettext(unquote(locale), unquote(domain), unquote(msgid), var!(bindings)) do
          unquote(compile_interpolation(msgstr))
        end
      end
    end
  end

  defp compile_translation(locale, domain, %PluralTranslation{} = t) do
    msgid        = IO.iodata_to_binary(t.msgid)
    msgid_plural = IO.iodata_to_binary(t.msgid_plural)

    msgstr = Enum.map(t.msgstr, fn {form, str} -> {form, IO.iodata_to_binary(str)} end)

    # If any of the msgstrs is empty, then we skip the generation of this
    # function clause. The reason we do this is the same as for the
    # `%Translation{}` clause.
    unless Enum.any?(msgstr, &match?({_, ""}, &1)) do
      clauses = Enum.map msgstr, fn({form, str}) ->
        {:->, [], [[form], compile_interpolation(str)]}
      end

      quote do
        def lngettext(unquote(locale), unquote(domain), unquote(msgid), unquote(msgid_plural), n, bindings) do
          # @plural_forms is defined in the current backend by
          # __before_compile__/1.
          plural_form    = @plural_forms.plural(unquote(locale), n)
          var!(bindings) = Map.put(bindings, :count, n)

          case plural_form, do: unquote(clauses)
        end
      end
    end
  end

  # Compiles a string into a full-blown `case` statement which interpolates the
  # string based on some bindings or returns an error in case those bindings are
  # missing. Note that the `bindings` variable is assumed to be in the scope by
  # the quoted code that is returned.
  defp compile_interpolation(str) do
    compile_interpolation(str, Interpolation.keys(str))
  end

  defp compile_interpolation(str, [] = _keys) do
    quote do
      _ = var!(bindings)
      {:ok, unquote(str)}
    end
  end

  defp compile_interpolation(str, keys) do
    match          = compile_interpolation_match(keys)
    interpolation  = compile_interpolatable_string(str)
    interpolatable = Interpolation.to_interpolatable(str)

    quote do
      case var!(bindings) do
        unquote(match) ->
          {:ok, unquote(interpolation)}
        %{} ->
          Gettext.Interpolation.interpolate(unquote(interpolatable), var!(bindings))
      end
    end
  end

  # Compiles a list of atoms into a "match" map. For example `[:foo, :bar]` gets
  # compiled to `%{foo: foo, bar: bar}`. All generated variables are under the
  # current `__MODULE__`.
  defp compile_interpolation_match(keys) do
    {:%{}, [], Enum.map(keys, &{&1, Macro.var(&1, __MODULE__)})}
  end

  # Compiles a string into a sequence of applications of the `<>` operator.
  # `%{var}` patterns are turned into `var` variables, namespaced inside the
  # current `__MODULE__`. Heavily inspired by Chris McCord's "linguist", see
  # https://github.com/chrismccord/linguist/blob/master/lib/linguist/compiler.ex#L70
  defp compile_interpolatable_string(str) do
    Enum.reduce Interpolation.to_interpolatable(str), "", fn
      key, acc when is_atom(key) ->
        quote do: unquote(acc) <> to_string(unquote(Macro.var(key, __MODULE__)))
      str, acc ->
        quote do: unquote(acc) <> unquote(str)
    end
  end

  # Returns all the PO files in `translations_dir` (under "canonical" paths,
  # i.e., `locale/LC_MESSAGES/domain.po`).
  defp po_files_in_dir(dir) do
    dir
    |> Path.join(@po_wildcard)
    |> Path.wildcard()
  end

  # Returns all the locales in `translations_dir` (which are the locales known
  # by the compiled backend).
  defp known_locales(translations_dir) do
    case File.ls(translations_dir) do
      {:ok, files} ->
        Enum.filter(files, &File.dir?(Path.join(translations_dir, &1)))
      {:error, :enoent} ->
        []
      {:error, reason} ->
        raise File.Error, reason: reason, action: "list directory", path: translations_dir
    end
  end
end
