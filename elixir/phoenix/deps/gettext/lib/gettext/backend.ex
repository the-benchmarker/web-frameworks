defmodule Gettext.Backend do
  @moduledoc """
  Behaviour that defines the macros that a Gettext backend has to implement.

  These macros are documented in great detail in the documentation for the
  `Gettext` module.
  """


  @doc """
  Default handling for missing bindings.

  This function is called when there are missing bindings in a translation. It
  takes a `Gettext.MissingBindingsError` struct and the translation with the
  wrong bindings left as is with the `%{}` syntax.

  For example, if something like this is called:

      MyApp.Gettext.gettext("Hello %{name}, welcome to %{country}", name: "Jane", country: "Italy")

  and our `it/LC_MESSAGES/default.po` looks like this:

      msgid "Hello %{name}, welcome to %{country}"
      msgstr "Ciao %{name}, benvenuto in %{cowntry}" # (typo)

  then Gettext will call:

      MyApp.Gettext.handle_missing_bindings(exception, "Ciao Jane, benvenuto in %{cowntry}")

  where `exception` is a struct that looks like this:

      %Gettext.MissingBindingsError{
        backend: MyApp.Gettext,
        domain: "default",
        locale: "it",
        msgid: "Hello %{name}, welcome to %{country}",
        bindings: [:country],
      }

  The return value of the `c:handle_missing_bindings/2` callback is used as the
  translated string that the translation macros and functions return.

  The default implementation for this function uses `Logger.error/1` to warn
  about the missing binding and returns the translated message with the
  incomplete bindings.

  This function can be overridden. For example, to raise when there are missing
  bindings:

      def handle_missing_bindings(exception, _incomplete) do
        raise exception
      end

  """
  @callback handle_missing_bindings(Gettext.MissingBindingsError.t, binary) ::
    binary | no_return

  @doc """
  Translates the given `msgid` in the given `domain`.

  `bindings` is a map of bindings to support interpolation.

  See also `Gettext.dgettext/4`.
  """
  @macrocallback dgettext(domain :: Macro.t, msgid :: String.t, bindings :: Macro.t) ::
    Macro.t

  @doc """
  Same as `dgettext(domain, msgid, %{})`.

  See also `Gettext.dgettext/4`.
  """
  @macrocallback dgettext(domain :: Macro.t, msgid :: String.t) :: Macro.t

  @doc """
  Same as `dgettext("default", msgid, %{})`.

  See also `Gettext.gettext/3`.
  """
  @macrocallback gettext(msgid :: String.t, bindings :: Macro.t) :: Macro.t

  @doc """
  Same as `gettext(msgid, %{})`.

  See also `Gettext.gettext/3`.
  """
  @macrocallback gettext(msgid :: String.t) :: Macro.t

  @doc """
  Translates the given plural translation (`msgid` + `msgid_plural`) in the
  given `domain`.

  `n` is an integer used to determine how to pluralize the
  translation. `bindings` is a map of bindings to support interpolation.

  See also `Gettext.dngettext/6`.
  """
  @macrocallback dngettext(domain :: Macro.t,
                           msgid :: String.t,
                           msgid_plural :: String.t,
                           n :: Macro.t,
                           bindings :: Macro.t) :: Macro.t

  @doc """
  Same as `dngettext(domain, msgid, msgid_plural, n, %{})`.

  See also `Gettext.dngettext/6`.
  """
  @macrocallback dngettext(domain :: Macro.t,
                           msgid :: String.t,
                           msgid_plural :: String.t,
                           n :: Macro.t) :: Macro.t

  @doc """
  Same as `dngettext("default", msgid, msgid_plural, n, bindings)`.

  See also `Gettext.ngettext/5`.
  """
  @macrocallback ngettext(msgid :: String.t,
                          msgid_plural :: String.t,
                          n :: Macro.t,
                          bindings :: Macro.t) :: Macro.t

  @doc """
  Same as `ngettext(msgid, msgid_plural, n, %{})`.

  See also `Gettext.ngettext/5`.
  """
  @macrocallback ngettext(msgid :: String.t,
                          msgid_plural :: String.t,
                          n :: Macro.t) :: Macro.t

  @doc """
  Marks the given translation for extraction and returns it unchanged.

  This macro can be used to mark a translation for extraction when `mix
  gettext.extract` is run. The return value is the given string, so that this
  macro can be used seamlessly in place of the string to extract.

  ## Examples

      MyApp.Gettext.dgettext_noop("errors", "Error found!")
      #=> "Error found!"

  """
  @macrocallback dgettext_noop(domain :: String.t, msgid :: String.t) :: Macro.t

  @doc """
  Same as `dgettext_noop("default", msgid)`.
  """
  @macrocallback gettext_noop(msgid :: String.t) :: Macro.t

  @doc """
  Marks the given translation for extraction and returns
  `{msgid, msgid_plural}`.

  This macro can be used to mark a translation for extraction when `mix
  gettext.extract` is run. The return value of this macro is `{msgid,
  msgid_plural}`.

  ## Examples

      my_fun = fn {msgid, msgid_plural} ->
        # do something with msgid and msgid_plural
      end

      my_fun.(MyApp.Gettext.dngettext_noop("errors", "One error", "%{count} errors"))

  """
  @macrocallback dngettext_noop(domain :: Macro.t,
                                msgid :: String.t,
                                msgid_plural :: String.t) :: Macro.t

  @doc """
  Same as `dngettext_noop("default", msgid, mgsid_plural)`.
  """
  @macrocallback ngettext_noop(msgid :: String.t, msgid_plural :: String.t) :: Macro.t
end
