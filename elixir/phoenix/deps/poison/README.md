# Poison

[![Travis](https://img.shields.io/travis/devinus/poison.svg?style=flat-square)](https://travis-ci.org/devinus/poison)
[![Hex.pm](https://img.shields.io/hexpm/v/poison.svg?style=flat-square)](https://hex.pm/packages/poison)
[![Hex.pm](https://img.shields.io/hexpm/dt/poison.svg?style=flat-square)](https://hex.pm/packages/poison)
[![Gratipay](https://img.shields.io/gratipay/devinus.svg?style=flat-square)](https://gratipay.com/devinus)

Poison is a new JSON library for Elixir focusing on wicked-fast **speed**
without sacrificing **simplicity**, **completeness**, or **correctness**.

Poison takes several approaches to be the fastest JSON library for Elixir.

Poison uses extensive [sub binary matching][1], a **hand-rolled parser** using
several techniques that are [known to benefit HiPE][2] for native compilation,
[IO list][3] encoding and **single-pass** decoding.

Preliminary benchmarking has sometimes put Poison's performance closer to
`jiffy`, and almost always faster than existing Elixir libraries.

## Installation

First, add Poison to your `mix.exs` dependencies:

```elixir
def deps do
  [{:poison, "~> 2.0"}]
end
```

Then, update your dependencies:

```sh-session
$ mix deps.get
```

## Usage

```elixir
defmodule Person do
  @derive [Poison.Encoder]
  defstruct [:name, :age]
end

Poison.encode!(%Person{name: "Devin Torres", age: 27})
#=> "{\"name\":\"Devin Torres\",\"age\":27}"

Poison.decode!(~s({"name": "Devin Torres", "age": 27}), as: %Person{})
#=> %Person{name: "Devin Torres", age: 27}

Poison.decode!(~s({"people": [{"name": "Devin Torres", "age": 27}]}),
  as: %{"people" => [%Person{}]})
#=> %{"people" => [%Person{age: 27, name: "Devin Torres"}]}
```

Every component of Poison -- the encoder, decoder, and parser -- are all usable
on their own without buying into other functionality. For example, if you were
interested purely in the speed of parsing JSON without a decoding step, you
could simply call `Poison.Parser.parse`.

If you use Poison 1.x, you have to set a module to `as` option in order to
decode into a struct. e.g. `as: Person` instead of `as: %Person{}`. The change was
introduced at 2.0.0.

## Parser

```iex
iex> Poison.Parser.parse!(~s({"name": "Devin Torres", "age": 27}))
%{"name" => "Devin Torres", "age" => 27}
iex> Poison.Parser.parse!(~s({"name": "Devin Torres", "age": 27}), keys: :atoms!)
%{name: "Devin Torres", age: 27}
```

Note that `keys: :atoms!` reuses existing atoms, i.e. if `:name` was not
allocated before the call, you will encounter an `argument error` message.

You can use the `keys: :atoms` variant to make sure all atoms are created as
needed.  However, unless you absolutely know what you're doing, do **not** do
it.  Atoms are not garbage-collected, see
[Erlang Efficiency Guide](http://www.erlang.org/doc/efficiency_guide/commoncaveats.html)
for more info:

> Atoms are not garbage-collected. Once an atom is created, it will never be
> removed. The emulator will terminate if the limit for the number of atoms
> (1048576 by default) is reached.

## Encoder

```iex
iex> IO.puts Poison.Encoder.encode([1, 2, 3], [])
"[1,2,3]"
```

Anything implementing the Encoder protocol is expected to return an
[IO list][4] to be embedded within any other Encoder's implementation and
passable to any IO subsystem without conversion.

```elixir
defimpl Poison.Encoder, for: Person do
  def encode(%{name: name, age: age}, options) do
    Poison.Encoder.BitString.encode("#{name} (#{age})", options)
  end
end
```

For maximum performance, make sure you `@derive [Poison.Encoder]` for any struct
you plan on encoding.

### Encoding only some attributes

When deriving structs for encoding, it is possible to select or exclude specific attributes. This is achieved by deriving `Poison.Encoder` with the `:only` or `:except` options set:

```elixir
defmodule PersonOnlyName do
  @derive {Poison.Encoder, only: [:name]}
  defstruct [:name, :age]
end

defmodule PersonWithoutName do
  @derive {Poison.Encoder, except: [:name]}
  defstruct [:name, :age]
end
```

In case both `:only` and `:except` keys are defined, the `:except` option is ignored.

## Benchmarking

```sh-session
$ mix deps.get
$ MIX_ENV=bench mix compile
$ MIX_ENV=bench mix bench
```

## License

Poison is released into the public domain (see `UNLICENSE`).
Poison is also optionally available under the ISC License (see `LICENSE`),
meant especially for jurisdictions that do not recognize public domain works.

[1]: http://www.erlang.org/euc/07/papers/1700Gustafsson.pdf
[2]: http://www.erlang.org/workshop/2003/paper/p36-sagonas.pdf
[3]: http://jlouisramblings.blogspot.com/2013/07/problematic-traits-in-erlang.html
[4]: http://prog21.dadgum.com/70.html
