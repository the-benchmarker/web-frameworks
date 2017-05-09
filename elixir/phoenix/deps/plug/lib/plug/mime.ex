defmodule Plug.MIME do
  @moduledoc false

  if Application.get_env(:plug, :mimes) do
    IO.puts :stderr, """
    warning: you have set the :mimes configuration for the :plug
    application but it is no longer supported. Instead of:

        config :plug, :mimes, %{...}

    You must write:

        config :mime, :types, %{...}

    After adding the configuration, MIME needs to be recompiled.
    If you are using mix, it can be done with:

        $ mix deps.clean mime --build
        $ mix deps.get

    """
  end

  def valid?(type) do
    # TODO: Enable warning
    # IO.puts(:stderr, "Plug.MIME.valid?/1 is deprecated, please use MIME.valid?/1 instead\n" <>
    #                  Exception.format_stacktrace)
    MIME.valid?(type)
  end

  def extensions(type) do
    # TODO: Enable warning
    # IO.puts(:stderr, "Plug.MIME.extensions/1 is deprecated, please use MIME.extensions/1 instead\n" <>
    #                  Exception.format_stacktrace)
    MIME.extensions(type)
  end

  def type(file_extension) do
    # TODO: Enable warning
    # IO.puts(:stderr, "Plug.MIME.type/1 is deprecated, please use MIME.type/1 instead\n" <>
    #                  Exception.format_stacktrace)
    MIME.type(file_extension)
  end

  def path(path) do
    # TODO: Enable warning
    # IO.puts(:stderr, "Plug.MIME.path/1 is deprecated, please use MIME.from_path/1 instead\n" <>
    #                  Exception.format_stacktrace)
    MIME.from_path(path)
  end
end
