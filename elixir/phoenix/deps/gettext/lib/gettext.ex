defmodule Gettext do
  @moduledoc ~S"""
  The `Gettext` module provides a
  [gettext](https://www.gnu.org/software/gettext/)-based API for working with
  internationalized applications.

  ## Using Gettext

  To use `Gettext`, a module that calls `use Gettext` has to be defined:

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app
      end

  This automatically defines some macros in the `MyApp.Gettext` module.
  Here are some examples:

      import MyApp.Gettext

      # Simple translation
      gettext "Here is the string to translate"

      # Plural translation
      ngettext "Here is the string to translate",
               "Here are the strings to translate",
               3

      # Domain-based translation
      dgettext "errors", "Here is the error message to translate"

  Translations are looked up from `.po` files. In the following sections we will
  explore exactly what are those files before we explore the "Gettext API" in
  detail.

  ## Translations

  Translations are stored inside PO (Portable Object) files, with a `.po`
  extension. For example, this is a snippet from a PO file:

      # This is a comment
      msgid "Hello world!"
      msgstr "Ciao mondo!"

  PO files containing translations for an application must be stored in a
  directory (by default it's `priv/gettext`) that has the following struture:

      gettext directory
      └─ locale
         └─ LC_MESSAGES
            ├─ domain_1.po
            ├─ domain_2.po
            └─ domain_3.po

  Here, `locale` is the locale of the translations (for example, `en_US`),
  `LC_MESSAGES` is a fixed directory, and `domain_i.po` are PO files containing
  domain-scoped translations. For more information on domains, check out the
  "Domains" section below.

  A concrete example of such a directory structure could look like this:

      priv/gettext
      └─ en_US
      |  └─ LC_MESSAGES
      |     ├─ default.po
      |     └─ errors.po
      └─ it
         └─ LC_MESSAGES
            ├─ default.po
            └─ errors.po

  By default, Gettext expects translations to be stored under the `priv/gettext`
  directory of an application. This behaviour can be changed by specifying a
  `:priv` option when using `Gettext`:

      # Look for translations in my_app/priv/translations instead of
      # my_app/priv/gettext
      use Gettext, otp_app: :my_app, priv: "priv/translations"

  The translations directory specified by the `:priv` option should be a directory
  inside `priv/`, otherwise some things (like `mix compile.gettext`) won't work
  as expected.

  ## Locale

  At runtime, all gettext-related functions and macros that do not explicitely
  take a locale as an argument read the locale from `Gettext.get_locale/1`. The
  locale can be set with `Gettext.put_locale/2`. Locales are expressed as
  strings (like `"en"` or `"fr"`); they can be arbitrary strings as long as they
  match a directory name.

  Gettext stores the locale **per-process** (in the process dictionary) and per
  Gettext module. This means that `Gettext.put_locale/2` must be called in every
  new process in order to have the right locale available in that process. Pay
  attention to this behaviour, since not setting the locale with
  `Gettext.put_locale/2` *will not* result in any errors when `Gettext.get_locale/1`
  is called; the default locale will be returned instead.

  ### Default locale

  The default Gettext locale is `"en"`. The value of the default locale for a
  given Gettext module can be set in the configuration for the `:otp_app` of
  that Gettext module. For example, in the `config/config.exs` file of the
  `my_app` application:

      config :my_app, MyApp.Gettext,
        default_locale: "fr"

  This option is read dynamically every time the locale has not been explicitly
  set, so to change the default locale of a backend for all processes at runtime
  it's enough to use `Application.put_env/3`.

  ## Gettext API

  There are two ways to use gettext:

    * using macros from your own gettext module, like `MyApp.Gettext`
    * using functions from the `Gettext` module

  These two approaches are different and each one has its own use case.

  ### Using macros

  Each module that calls `use Gettext` is usually referred to as a "Gettext
  backend", as it implements the `Gettext.Backend` behaviour. When a module
  calls `use Gettext`, the following macros are automatically
  defined inside it:

    * `gettext/2`
    * `dgettext/3`
    * `ngettext/4`
    * `dngettext/5`
    * `gettext_noop/1`, `dgettext_noop/2`, `ngettext_noop/3`, `dngettext_noop/4`

  Supposing the caller module is `MyApp.Gettext`, the macros mentioned above
  behave as follows:

    * `gettext(msgid, bindings \\ %{})` -
      like `Gettext.gettext(MyApp.Gettext, msgid, bindings)`
    * `dgettext(domain, msgid, bindings \\ %{})` -
      like `Gettext.dgettext(MyApp.Gettext, domain, msgid, bindings)`
    * `ngettext(msgid, msgid_plural, n, bindings \\ %{})` -
      like `Gettext.ngettext(MyApp.Gettext, msgid, msgid_plural, n, bindings)`
    * `dngettext(domain, msgid, msgid_plural, n, bindings \\ %{})` -
      like `Gettext.dngettext(MyApp.Gettext, domain, msgid, msgid_plural, n, bindings)`
    * `*_noop` family of functions - used to mark translations for extraction
      without translating them; see the documentation for these macros in
      `Gettext.Backend`

  See also the `Gettext.Backend` behaviour for more detailed documentation about
  these macros.

  Using macros is preferred as gettext is able to automatically sync the
  translations in your code with PO files. This, however, imposes a constraint:
  arguments passed to any of these macros have to be strings **at compile
  time**. This means that they have to be string literals or something that
  expands to a string literal at compile time (e.g., a module attribute like
  `@my_string "foo"`).

  These are all valid uses of the gettext macros:

      Gettext.put_locale MyApp.Gettext, "it"

      MyApp.Gettext.gettext "Hello world"
      #=> "Ciao mondo"

      @msgid "Hello world"
      MyApp.Gettext.gettext @msgid
      #=> "Ciao mondo"

  The `gettext`/`dgettext`/`ngettext`/`dngettext` macros raise an
  `ArgumentError` exception if they receive a `domain`, `msgid`, or
  `msgid_plural` that doesn't expand to a string at compile time:

      msgid = "Hello world"
      MyApp.Gettext.gettext msgid
      #=> ** (ArgumentError) msgid must be a string literal

  Using compile-time strings isn't always possible. For this reason,
  the `Gettext` module provides a set of functions as well.

  ### Using functions

  If compile-time strings cannot be used, the solution is to use the functions
  in the `Gettext` module instead of the macros described above. These functions
  perfectly mirror the macro API, but they all expect a module name as the first
  argument. This module has to be a module which calls `use Gettext`. For example:

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app
      end

      Gettext.put_locale MyApp.Gettext, "pt_BR"

      msgid = "Hello world"
      Gettext.gettext(MyApp.Gettext, msgid)
      #=> "Olá mundo"

  While using functions from the `Gettext` module yields the same results as
  using macros (with the added benefit of dynamic arguments), all the
  compile-time features mentioned in the previous section are lost.

  ## Domains

  The `dgettext` and `dngettext` functions/macros also accept a *domain* as one
  of the arguments. The domain of a translation is determined by the name of the
  PO file that contains that translation. For example, the domain of
  translations in the `it/LC_MESSAGES/errors.po` file is `"errors"`, so those
  translations would need to be retrieved with `dgettext` or `dngettext`:

      MyApp.Gettext.dgettext "errors", "Error!"
      #=> "Errore!"

  When `gettext` or `ngettext` are used, the `"default"` domain is used.

  ## Interpolation

  All `*gettext` functions and macros provided by gettext support interpolation.
  Interpolation keys can be placed in `msgid`s or `msgid_plural`s with by
  enclosing them in `%{` and `}`, like this:

      "This is an %{interpolated} string"

  Interpolation bindings can be passed as an argument to all of the `*gettext`
  functions/macros. For example, given the following PO file for the `"it"`
  locale:

      msgid "Hello, %{name}!"
      msgstr "Ciao, %{name}!"

  interpolation can be done like follows:

      Gettext.put_locale MyApp.Gettext, "it"
      MyApp.Gettext.gettext "Hello, %{name}!", name: "Meg"
      #=> "Ciao, Meg!"

  Interpolation keys that are in a string but not in the provided bindings
  result in a `Gettext.Error` exception:

      MyApp.Gettext.gettext "Hello, %{name}!"
      #=> ** (Gettext.Error) missing interpolation keys: name

  Keys that are in the interpolation bindings but that don't occur in the string
  are ignored. Interpolations in gettext are often expanded at compile time,
  ensuring a low performance cost when running them at runtime.

  ## Pluralization

  Pluralization in gettext for Elixir works very similar to how pluralization
  works in GNU gettext. The `*ngettext` functions/macros accept a `msgid`, a
  `msgid_plural` and a count of elements; the right translation is chosen based
  on the **pluralization rule** for the given locale.

  For example, given the following snippet of PO file for the `"it"` locale:

      msgid "One error"
      msgid_plural "%{count} errors"
      msgstr[0] "Un errore"
      msgstr[1] "%{count} errori"

  the `ngettext` macro can be used like this:

      Gettext.put_locale MyApp.Gettext, "it"
      MyApp.Gettext.ngettext "One error", "%{count} errors", 3
      #=> "3 errori"

  The `%{count}` interpolation key is a special key since it gets replaced by
  the number of elements argument passed to `*ngettext`, like if the `count: 3`
  key-value pair were in the interpolation bindings. Hence, never pass the
  `count` key in the bindings:

      # `count: 4` is ignored here
      MyApp.Gettext.ngettext "One error", "%{count} errors", 3, count: 4
      #=> "3 errori"

  You can specify a "pluralizer" module via the `:plural_forms` option in the
  configuration for each Gettext backend.

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app, plural_forms: MyApp.PluralForms
      end

  To learn more about pluralization rules, plural forms and what they mean to
  Gettext check the documentation for `Gettext.Plural`.

  ## Missing translations

  When a translation is missing in the specified locale (both with functions as
  well as with macros), the argument is returned:

    * in case of calls to `gettext`/`dgettext`, the `msgid` argument is returned
      as is;
    * in case of calls to `ngettext`/`dngettext`, the `msgid` argument is
      returned in case of a singular value and the `msgid_plural` is returned in
      case of a plural value (following the English pluralization rule).

  For example:

      Gettext.put_locale MyApp.Gettext, "foo"
      MyApp.Gettext.gettext "Hey there"
      #=> "Hey there"
      MyApp.Gettext.ngettext "One error", "%{count} errors", 3
      #=> "3 errors"

  ### Empty translations

  When a `msgstr` is empty (`""`), the translation is considered missing and the
  behaviour described above for missing translation is applied. A plural
  translation is considered to have an empty `msgstr` if at least one
  translation in the `msgstr` is empty.

  ## Contexts

  The GNU Gettext implementation supports
  [*contexts*](https://www.gnu.org/software/gettext/manual/html_node/Contexts.html),
  which are a way to "contextualize" translations. For example, in English, the
  word "file" could be used both as a noun or as a verb. Contexts can be used to
  solve similar problems: one could have a "imperative_verbs" context and a
  "nouns" context as to avoid ambiguity. However, contexts increase the
  complexity of Gettext and would increase the complexity of the implementation
  of Gettext for Elixir, and for this reason we decided to not support them. The
  problem they try to solve can still be solved just using domains: for example,
  one could have the `default-imperative_verbs` domain and the `default-nouns`
  domain and use the `d(n)gettext` family of macros/functions, and the final
  result would be similar

  ## Compile-time features

  As mentioned above, using the gettext macros (as opposed to functions) allows
  gettext to operate on those translations *at compile-time*. This can be used
  to extract translations from the source code into POT files automatically
  (instead of having to manually add translations to POT files when they're added
  to the source code). The `gettext.extract` does exactly this: whenever there
  are new translations in the source code, running `gettext.extract` syncs the
  existing POT files with the changed code base. Read the documentation for
  `Mix.Tasks.Gettext.Extract` for more information on the extraction process.

  POT files are just *template* files and the translations in them do not
  actually contain translated strings. A POT file looks like this:

      # The msgstr is empty
      msgid "hello, world"
      msgstr ""

  Whenever a POT file changes, it's likely that developers (or translators) will
  want to update the corresponding PO files for each locale. To do that, gettext
  provides the `gettext.merge` Mix task. For example, running:

      mix gettext.merge priv/gettext --locale pt_BR

  will update all the PO files in `priv/gettext/pt_BR/LC_MESSAGES` with the new
  version of the POT files in `priv/gettext`. Read more about the merging
  process in the documentation for `Mix.Tasks.Gettext.Merge`.

  Finally, gettext is able to recompile modules that call `use Gettext` whenever
  PO files change. To enable this feature, the `:gettext` compiler needs to be
  added to the list of Mix compilers. In `mix.exs`:

      def project do
        [compilers: [:gettext] ++ Mix.compilers]
      end

  ## Configuration

  ### Backend configuration

  A **Gettext backend** supports some options to be configured. These options
  can be configured in two ways: either by passing them to `use Gettext` (hence
  at compile time):

      defmodule MyApp.Gettext do
        use Gettext, options
      end

  or by using Mix configuration, configuring the key corresponding to the
  backend in the configuration for your application:

      # For example, in config/config.exs
      config :my_app, MyApp.Gettext, options

  Note that the `:otp_app` option (an atom representing an OTP application) has
  to always be present and has to be passed to `use Gettext` because it's used
  to determine the application to read the configuration of (`:my_app` in the
  example above); for this reason, `:otp_app` can't be configured via the Mix
  configuration. This option is also used to determine the application's
  directory where to search translations in.

  The following is a comprehensive list of supported options:

    * `:priv` - a string representing a directory where translations will be
      searched. The directory is relative to the directory of the application
      specified by the `:otp_app` option. It is recommended to always have
      this directory inside `"priv"`, otherwise some features like the
      "mix compile.gettext" won't work as expected. By default it's
      `"priv/gettext"`.

    * `:plural_forms` - a module which will act as a "pluralizer". For more
      information, look at the documentation for `Gettext.Plural`.

    * `:default_locale` - a string which specifies the default locale to use for
      the given backend.

  ### Mix tasks configuration

  You can configure Gettext Mix tasks under the `:gettext` key in the
  configuration returned by `project/0` in `mix.exs`:

      def project() do
        [app: :my_app,
         # ...
         gettext: [...]]
      end

  The following is a list of the supported configuration options:

    * `:fuzzy_threshold` - the default threshold for the Jaro distance measuring
      the similarity of translations. Look at the documentation for the `mix
      gettext.merge` task (`Mix.Tasks.Gettext.Merge`) for more information on
      fuzzy translations.

    * `:excluded_refs_from_purging` - a regex that is matched against translation
      references. Gettext will preserve all translations in all POT files that
      have a matching reference. You can use this pattern to prevent Gettext from
      removing translations that you have extracted using another tool.

    * `:compiler_po_wildcard` - a binary that specifies the wildcard that the
      `:gettext` compiler will use to find changed PO files in order to recompile
      their respective Gettext backends. This wildcard has to be relative to the
      `"priv"` directory of your application. Defaults to
      `"gettext/*/LC_MESSAGES/*.po"`.

  """

  defmodule Error do
    @moduledoc """
    A generic error raised for a variety of possible Gettext-related reasons
    (e.g., missing interpolation keys).
    """
    defexception [:message]
  end

  defmodule MissingBindingsError do
    @moduledoc """
    An error message raised for missing bindings errors.
    """
    @enforce_keys [:backend, :domain, :locale, :msgid, :missing]
    defexception [:backend, :domain, :locale, :msgid, :missing]

    def message(%{backend: backend, domain: domain, locale: locale, msgid: msgid, missing: missing}) do
      "missing Gettext bindings: #{inspect missing} (backend #{inspect backend}, " <>
        "locale #{inspect locale}, domain #{inspect domain}, msgid #{inspect msgid})"
    end
  end

  @type locale :: binary
  @type backend :: module
  @type bindings :: %{} | Keyword.t

  @doc false
  defmacro __using__(opts) do
    quote do
      require Logger

      @gettext_opts unquote(opts)
      @before_compile Gettext.Compiler

      def handle_missing_bindings(exception, incomplete) do
        _ = Logger.error(Exception.message(exception))
        incomplete
      end
      defoverridable handle_missing_bindings: 2
    end
  end

  @doc """
  Gets the locale for the current process and the given backend.

  This function returns the value of the locale for the current process and the
  given `backend`. If there is no locale for the current process and the given
  backend, the default locale is set as the locale for the current process and
  the given backend and then returned. For more information on the default
  locale and how it can be set, refer to the documentation of the `Gettext`
  module.

  ## Examples

      Gettext.get_locale(MyApp.Gettext)
      #=> "en"

  """
  @spec get_locale(backend) :: locale
  def get_locale(backend) do
    if locale = Process.get(backend) do
      locale
    else
      backend_config = Application.get_env(backend.__gettext__(:otp_app), backend, [])
      default_locale = Keyword.get(backend_config, :default_locale, "en")
      Process.put(backend, default_locale)
      default_locale
    end
  end

  @doc """
  Sets the locale for the current process and the given `backend`.

  The locale is stored in the process dictionary. `locale` must be a string; if
  it's not, an `ArgumentError` exception is raised.

  ## Examples

      Gettext.put_locale(MyApp.Gettext, "pt_BR")
      #=> nil
      Gettext.get_locale(MyApp.Gettext)
      #=> "pt_BR"

  """
  @spec put_locale(backend, locale) :: nil
  def put_locale(backend, locale) when is_binary(locale),
    do: Process.put(backend, locale)
  def put_locale(_, locale),
    do: raise(ArgumentError, "put_locale/2 only accepts binary locales, got: #{inspect locale}")

  @doc """
  Returns the translation of the given string in the given domain.

  The string is translated by the `backend` module.

  The translated string is interpolated based on the `bindings` argument. For
  more information on how interpolation works, refer to the documentation of the
  `Gettext` module.

  If the translation for the given `msgid` is not found, the `msgid`
  (interpolated if necessary) is returned.

  ## Examples

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app
      end

      Gettext.put_locale(MyApp.Gettext, "it")

      Gettext.dgettext(MyApp.Gettext, "errors", "Invalid")
      #=> "Non valido"

      Gettext.dgettext(MyApp.Gettext, "errors", "%{name} is not a valid name", name: "Meg")
      #=> "Meg non è un nome valido"

      Gettext.dgettext(MyApp.Gettext, "alerts", "nonexisting")
      #=> "nonexisting"

  """
  @spec dgettext(module, binary, binary, bindings) :: binary
  def dgettext(backend, domain, msgid, bindings \\ %{})

  def dgettext(backend, domain, msgid, bindings) when is_list(bindings) do
    dgettext(backend, domain, msgid, :maps.from_list(bindings))
  end

  def dgettext(backend, domain, msgid, bindings)
      when is_atom(backend) and is_binary(domain) and is_binary(msgid) and is_map(bindings) do
    locale = get_locale(backend)
    backend.lgettext(locale, domain, msgid, bindings)
    |> handle_backend_result(backend, locale, domain, msgid)
  end

  @doc """
  Returns the translation of the given string in the `"default"` domain.

  Works exactly like:

      Gettext.dgettext(backend, "default", msgid, bindings)

  """
  @spec gettext(module, binary, bindings) :: binary
  def gettext(backend, msgid, bindings \\ %{}) do
    dgettext(backend, "default", msgid, bindings)
  end

  @doc """
  Returns the pluralized translation of the given string in the given domain.

  The string is translated and pluralized by the `backend` module.

  The translated string is interpolated based on the `bindings` argument. For
  more information on how interpolation works, refer to the documentation of the
  `Gettext` module.

  If the translation for the given `msgid` and `msgid_plural` is not found, the
  `msgid` or `msgid_plural` (based on `n` being singular or plural) is returned
  (interpolated if necessary).

  ## Examples

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app
      end

      Gettext.dngettext(MyApp.Gettext, "errors", "Error", "%{count} errors", 3)
      #=> "3 errori"
      Gettext.dngettext(MyApp.Gettext, "errors", "Error", "%{count} errors", 1)
      #=> "Errore"

  """
  @spec dngettext(module, binary, binary, binary, non_neg_integer, bindings) :: binary
  def dngettext(backend, domain, msgid, msgid_plural, n, bindings \\ %{})

  def dngettext(backend, domain, msgid, msgid_plural, n, bindings) when is_list(bindings) do
    dngettext(backend, domain, msgid, msgid_plural, n, :maps.from_list(bindings))
  end

  def dngettext(backend, domain, msgid, msgid_plural, n, bindings)
      when is_atom(backend) and is_binary(domain) and is_binary(msgid) and
           is_binary(msgid_plural) and is_integer(n) and n >= 0 and
           is_map(bindings) do
    locale = get_locale(backend)
    backend.lngettext(locale, domain, msgid, msgid_plural, n, bindings)
    |> handle_backend_result(backend, locale, domain, msgid)
  end

  @doc """
  Returns the pluralized translation of the given string in the `"default"`
  domain.

  Works exactly like:

      Gettext.dngettext(backend, "default", msgid, msgid_plural, n, bindings)

  """
  @spec ngettext(module, binary, binary, non_neg_integer, bindings) :: binary
  def ngettext(backend, msgid, msgid_plural, n, bindings \\ %{}) do
    dngettext(backend, "default", msgid, msgid_plural, n, bindings)
  end

  @doc """
  Runs `fun` with the gettext locale set to `locale` for the given `backend`.

  This function just sets the Gettext locale for `backend` to `locale` before
  running `fun` and sets it back to its previous value afterwards. Note that
  `put_locale/2` is used to set the locale, which is thus set only for the
  current process (keep this in mind if you plan on spawning processes inside
  `fun`).

  The value returned by this function is the return value of `fun`.

  ## Examples

      Gettext.put_locale(MyApp.Gettext, "fr")

      MyApp.Gettext.gettext("Hello world")
      #=> "Bonjour monde"

      Gettext.with_locale MyApp.Gettext, "it", fn ->
        MyApp.Gettext.gettext("Hello world")
      end
      #=> "Ciao mondo"

      MyApp.Gettext.gettext("Hello world")
      #=> "Bonjour monde"

  """
  @spec with_locale(backend, locale, (() -> result)) :: result when result: var
  def with_locale(backend, locale, fun) do
    previous_locale = Gettext.get_locale(backend)
    Gettext.put_locale(backend, locale)

    try do
      fun.()
    after
      Gettext.put_locale(backend, previous_locale)
    end
  end

  @doc """
  Returns all the locales for which PO files exist for the given `backend`.

  If the translations directory for the given backend doesn't exist, then an
  empty list is returned.

  ## Examples

  With the following backend:

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app
      end

  and the following translations directory:

      my_app/priv/gettext
      ├─ en
      ├─ it
      └─ pt_BR

  then:

      Gettext.known_locales(MyApp.Gettext)
      #=> ["en", "it", "pt_BR"]

  """
  @spec known_locales(backend) :: [locale]
  def known_locales(backend) do
    backend.__gettext__(:known_locales)
  end

  defp handle_backend_result({:ok, string}, _backend, _locale, _domain, _msgid),
    do: string
  defp handle_backend_result({:default, string}, _backend, _locale, _domain, _msgid),
    do: string
  defp handle_backend_result({:missing_bindings, incomplete, missing}, backend, locale, domain, msgid) do
    exception = %MissingBindingsError{backend: backend, locale: locale, domain: domain,
                                      msgid: msgid, missing: missing}
    backend.handle_missing_bindings(exception, incomplete)
  end
  defp handle_backend_result({:error, reason}, _backend, _locale, _domain, _msgid) do
    raise Error, reason
  end
end
