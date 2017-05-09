defmodule Gettext.Plural do
  @moduledoc """
  Behaviour and default implementation for finding plural forms in given
  locales.

  This module both defines the `Gettext.Plural` behaviour and provides a default
  implementation for it.

  ## Plural forms

  > For a given language, there is a grammatical rule on how to change words
  > depending on the number qualifying the word. Different languages can have
  > different rules.
  [[source]](https://developer.mozilla.org/en-US/docs/Mozilla/Localization/Localization_and_Plurals)

  Such grammatical rules define a number of **plural forms**. For example,
  English has two plural forms: one for when there is just one element (the
  *singular*) and another one for when there are zero or more than one elements
  (the *plural*). There are languages which only have one plural form and there
  are languages which have more than two.

  In GNU Gettext (and in Gettext for Elixir), plural forms are represented by
  increasing 0-indexed integers. For example, in English `0` means singular and
  `1` means plural.

  The goal of this module is to determine, given a locale:

    * how many plural forms exist in that locale (`nplurals/1`);
    * to what plural form a given number of elements belongs to in that locale
      (`plural/2`).

  ## Default implementation

  `Gettext.Plural` provides a default implementation of a plural module. Most
  languages used on Earth should be covered by this default implementation. If
  custom pluralization rules are needed (for example, to add additional
  languages) a different plural module can be specified when creating a Gettext
  backend. For example, pluralization rules for the Elvish language could be
  added as follows:

      defmodule MyApp.Plural do
        @behaviour Gettext.Plural

        def nplurals("elv"), do: 3

        def plural("elv", 0), do: 0
        def plural("elv", 1), do: 1
        def plural("elv", _), do: 2

        # Fallback to Gettext.Plural
        def nplurals(locale), do: Gettext.Plural.nplurals(locale)
        def plural(locale, n), do: Gettext.Plural.plural(locale, n)
      end

      defmodule MyApp.Gettext do
        use Gettext, otp_app: :my_app, plural_forms: MyApp.Plural
      end

  The mathematical expressions used in this module to determine the plural form
  of a given number of elements are taken from [this
  page](http://localization-guide.readthedocs.org/en/latest/l10n/pluralforms.html#f2)
  as well as from [Mozilla's guide on "Localization and
  plurals"](https://developer.mozilla.org/en-US/docs/Mozilla/Localization/Localization_and_Plurals).

  ### Unknown locales

  Trying to call `Gettext.Plural` functions with unknown locales will result in
  a `UnknownLocaleError` exception.

  ### Language and country

  Often, a locale is composed as a language and country couple, such as
  `en_US`. The default implementation for `Gettext.Plural` handles `xx_YY` by
  forwarding it to `xx` (except for *just Brazilian Portuguese*, `pt_BR`, which
  is not forwarded to `pt` as pluralization rules slightly differ). We treat the
  underscore as a separator according to ISO 15897. Sometimes, a dash `-` is
  used as a separator (e.g., `en-US`): this is not forwarded to `en` in the
  default `Gettext.Plural` (and it will raise an `Gettext.Plural.UnknownLocaleError`
  exception).

  ## Examples

  An example of the plural form of a given number of elements in the Polish
  language:

      iex> Plural.plural("pl", 1)
      0
      iex> Plural.plural("pl", 2)
      1
      iex> Plural.plural("pl", 5)
      2
      iex> Plural.plural("pl", 112)
      2

  As expected, `nplurals/1` returns the possible number of plural forms:

      iex> Plural.nplurals("pl")
      3

  """

  # Behaviour definition.

  @doc """
  Returns the number of possible plural forms in the given `locale`.
  """
  @callback nplurals(locale :: String.t) :: pos_integer

  @doc """
  Returns the plural form in the given `locale` for the given `count` of
  elements.
  """
  @callback plural(locale :: String.t, count :: integer) :: (plural_form :: non_neg_integer)

  defmodule UnknownLocaleError do
    @moduledoc """
    Raised when a pluralized module doesn't know how to handle a locale.

    ## Examples

        raise Gettext.Plural.UnknownLocaleError, "en-US"

    """
    defexception [:message]

    def exception(locale) when is_binary(locale) do
      %__MODULE__{message: """
      unknown locale #{inspect locale}. If this is a locale you need to handle,
      consider using a custom pluralizer module instead of the default
      Gettext.Plural. You can read more about this on the Gettext docs at
      https://hexdocs.pm/gettext/Gettext.Plural.html
      """}
    end
  end

  # Behaviour implementation.

  defmacrop ends_in(n, digits) do
    digits = List.wrap(digits)
    quote do
      rem(unquote(n), 10) in unquote(digits)
    end
  end

  @one_form [
    "ay",  # Aymará
    "bo",  # Tibetan
    "cgg", # Chiga
    "dz",  # Dzongkha
    "fa",  # Persian
    "id",  # Indonesian
    "ja",  # Japanese
    "jbo", # Lojban
    "ka",  # Georgian
    "kk",  # Kazakh
    "km",  # Khmer
    "ko",  # Korean
    "ky",  # Kyrgyz
    "lo",  # Lao
    "ms",  # Malay
    "my",  # Burmese
    "sah", # Yakut
    "su",  # Sundanese
    "th",  # Thai
    "tt",  # Tatar
    "ug",  # Uyghur
    "vi",  # Vietnamese
    "wo",  # Wolof
    "zh",  # Chinese [2]
  ]

  @two_forms_1 [
    "af",    # Afrikaans
    "an",    # Aragonese
    "anp",   # Angika
    "as",    # Assamese
    "ast",   # Asturian
    "az",    # Azerbaijani
    "bg",    # Bulgarian
    "bn",    # Bengali
    "brx",   # Bodo
    "ca",    # Catalan
    "da",    # Danish
    "de",    # German
    "doi",   # Dogri
    "el",    # Greek
    "en",    # English
    "eo",    # Esperanto
    "es",    # Spanish
    "et",    # Estonian
    "eu",    # Basque
    "ff",    # Fulah
    "fi",    # Finnish
    "fo",    # Faroese
    "fur",   # Friulian
    "fy",    # Frisian
    "gl",    # Galician
    "gu",    # Gujarati
    "ha",    # Hausa
    "he",    # Hebrew
    "hi",    # Hindi
    "hne",   # Chhattisgarhi
    "hy",    # Armenian
    "hu",    # Hungarian
    "ia",    # Interlingua
    "it",    # Italian
    "kl",    # Greenlandic
    "kn",    # Kannada
    "ku",    # Kurdish
    "lb",    # Letzeburgesch
    "mai",   # Maithili
    "ml",    # Malayalam
    "mn",    # Mongolian
    "mni",   # Manipuri
    "mr",    # Marathi
    "nah",   # Nahuatl
    "nap",   # Neapolitan
    "nb",    # Norwegian Bokmal
    "ne",    # Nepali
    "nl",    # Dutch
    "se",    # Northern Sami
    "nn",    # Norwegian Nynorsk
    "no",    # Norwegian (old code)
    "nso",   # Northern Sotho
    "or",    # Oriya
    "ps",    # Pashto
    "pa",    # Punjabi
    "pap",   # Papiamento
    "pms",   # Piemontese
    "pt",    # Portuguese
    "rm",    # Romansh
    "rw",    # Kinyarwanda
    "sat",   # Santali
    "sco",   # Scots
    "sd",    # Sindhi
    "si",    # Sinhala
    "so",    # Somali
    "son",   # Songhay
    "sq",    # Albanian
    "sw",    # Swahili
    "sv",    # Swedish
    "ta",    # Tamil
    "te",    # Telugu
    "tk",    # Turkmen
    "ur",    # Urdu
    "yo",    # Yoruba
  ]

  @two_forms_2 [
    "ach",   # Acholi
    "ak",    # Akan
    "am",    # Amharic
    "arn",   # Mapudungun
    "br",    # Breton
    "fil",   # Filipino
    "fr",    # French
    "gun",   # Gun
    "ln",    # Lingala
    "mfe",   # Mauritian Creole
    "mg",    # Malagasy
    "mi",    # Maori
    "oc",    # Occitan
    "tg",    # Tajik
    "ti",    # Tigrinya
    "tl",    # Tagalog
    "tr",    # Turkish
    "uz",    # Uzbek
    "wa",    # Walloon
  ]

  @three_forms_slavic [
    "be", # Belarusian
    "bs", # Bosnian
    "hr", # Croatian
    "sr", # Serbian
    "ru", # Russian
    "uk", # Ukrainian
  ]

  @three_forms_slavic_alt [
    "cs", # Czech
    "sk", # Slovak
  ]

  # Number of plural forms.

  def nplurals(locale)

  # All the groupable forms.

  for l <- @one_form do
    def nplurals(unquote(l)), do: 1
  end

  for l <- @two_forms_1 ++ @two_forms_2 do
    def nplurals(unquote(l)), do: 2
  end

  for l <- @three_forms_slavic ++ @three_forms_slavic_alt do
    def nplurals(unquote(l)), do: 3
  end

  # Then, all other ones.

  # Arabic
  def nplurals("ar"), do: 6

  # Kashubian
  def nplurals("csb"), do: 3

  # Welsh
  def nplurals("cy"), do: 4

  # Irish
  def nplurals("ga"), do: 5

  # Scottish Gaelic
  def nplurals("gd"), do: 4

  # Icelandic
  def nplurals("is"), do: 2

  # Javanese
  def nplurals("jv"), do: 2

  # Cornish
  def nplurals("kw"), do: 4

  # Lithuanian
  def nplurals("lt"), do: 3

  # Latvian
  def nplurals("lv"), do: 3

  # Macedonian
  def nplurals("mk"), do: 3

  # Mandinka
  def nplurals("mnk"), do: 3

  # Maltese
  def nplurals("mt"), do: 4

  # Polish
  def nplurals("pl"), do: 3

  # Romanian
  def nplurals("ro"), do: 3

  # Slovenian
  def nplurals("sl"), do: 4

  # Match-all clause.
  def nplurals(locale) do
    recall_if_country_or_raise(locale, &nplurals/1)
  end

  # Plural form of groupable languages.

  def plural(locale, count)

  # All the `x_Y` languages that have different pluralization rules than `x`.

  def plural("pt_BR", n) when n in [0, 1], do: 0
  def plural("pt_BR", _n), do: 1

  # Groupable forms.

  for l <- @one_form do
    def plural(unquote(l), _n), do: 0
  end

  for l <- @two_forms_1 do
    def plural(unquote(l), 1), do: 0
    def plural(unquote(l), _n), do: 1
  end

  for l <- @two_forms_2 do
    def plural(unquote(l), n) when n in [0, 1], do: 0
    def plural(unquote(l), _n), do: 1
  end

  for l <- @three_forms_slavic do
    def plural(unquote(l), n)
      when ends_in(n, 1) and rem(n, 100) != 11,
      do: 0
    def plural(unquote(l), n)
      when ends_in(n, [2, 3, 4]) and (rem(n, 100) < 10 or rem(n, 100) >= 20),
      do: 1
    def plural(unquote(l), _n),
      do: 2
  end

  for l <- @three_forms_slavic_alt do
    def plural(unquote(l), 1), do: 0
    def plural(unquote(l), n) when n in 2..4, do: 1
    def plural(unquote(l), _n), do: 2
  end

  # Custom plural forms.

  # Arabic
  # n==0 ? 0 : n==1 ? 1 : n==2 ? 2 : n%100>=3 && n%100<=10 ? 3 : n%100>=11 ? 4 : 5
  def plural("ar", 0), do: 0
  def plural("ar", 1), do: 1
  def plural("ar", 2), do: 2
  def plural("ar", n) when rem(n, 100) >= 3 and rem(n, 100) <= 10, do: 3
  def plural("ar", n) when rem(n, 100) >= 11, do: 4
  def plural("ar", _n), do: 5

  # Kashubian
  # (n==1) ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;
  def plural("csb", 1),
    do: 0
  def plural("csb", n)
    when ends_in(n, [2, 3, 4]) and (rem(n, 100) < 10 or rem(n, 100) >= 20),
    do: 1
  def plural("csb", _n),
    do: 2

  # Welsh
  # (n==1) ? 0 : (n==2) ? 1 : (n != 8 && n != 11) ? 2 : 3
  def plural("cy", 1), do: 0
  def plural("cy", 2), do: 1
  def plural("cy", n) when n != 8 and n != 11, do: 2
  def plural("cy", _n), do: 3

  # Irish
  # n==1 ? 0 : n==2 ? 1 : (n>2 && n<7) ? 2 :(n>6 && n<11) ? 3 : 4
  def plural("ga", 1), do: 0
  def plural("ga", 2), do: 1
  def plural("ga", n) when n in 3..6, do: 2
  def plural("ga", n) when n in 7..10, do: 3
  def plural("ga", _n), do: 4

  # Scottish Gaelic
  # (n==1 || n==11) ? 0 : (n==2 || n==12) ? 1 : (n > 2 && n < 20) ? 2 : 3
  def plural("gd", n) when n == 1 or n == 11, do: 0
  def plural("gd", n) when n == 2 or n == 12, do: 1
  def plural("gd", n) when n > 2 and n < 20, do: 2
  def plural("gd", _n), do: 3

  # Icelandic
  # n%10!=1 || n%100==11
  def plural("is", n) when ends_in(n, 1) and rem(n, 100) != 11, do: 0
  def plural("is", _n), do: 1

  # Javanese
  # n != 0
  def plural("jv", 0), do: 0
  def plural("jv", _), do: 1

  # Cornish
  # (n==1) ? 0 : (n==2) ? 1 : (n == 3) ? 2 : 3
  def plural("kw", 1), do: 0
  def plural("kw", 2), do: 1
  def plural("kw", 3), do: 2
  def plural("kw", _), do: 3

  # Lithuanian
  # n%10==1 && n%100!=11 ? 0 : n%10>=2 && (n%100<10 || n%100>=20) ? 1 : 2
  def plural("lt", n)
    when ends_in(n, 1) and rem(n, 100) != 11,
    do: 0
  def plural("lt", n)
    when rem(n, 10) >= 2 and (rem(n, 100) < 10 or rem(n, 100) >= 20),
    do: 1
  def plural("lt", _),
    do: 2

  # Latvian
  # n%10==1 && n%100!=11 ? 0 : n != 0 ? 1 : 2
  def plural("lv", n) when ends_in(n, 1) and rem(n, 100) != 11, do: 0
  def plural("lv", n) when n != 0, do: 1
  def plural("lv", _), do: 2

  # Macedonian
  # n==1 || n%10==1 ? 0 : 1; Can’t be correct needs a 2 somewhere
  def plural("mk", n) when ends_in(n, 1), do: 0
  def plural("mk", n) when ends_in(n, 2), do: 1
  def plural("mk", _), do: 2

  # Mandinka
  # n==0 ? 0 : n==1 ? 1 : 2
  def plural("mnk", 0), do: 0
  def plural("mnk", 1), do: 1
  def plural("mnk", _), do: 2

  # Maltese
  # n==1 ? 0 : n==0 || ( n%100>1 && n%100<11) ? 1 : (n%100>10 && n%100<20 ) ? 2 : 3
  def plural("mt", 1), do: 0
  def plural("mt", n) when n == 0 or (rem(n, 100) > 1 and rem(n, 100) < 11), do: 1
  def plural("mt", n) when rem(n, 100) > 10 and rem(n, 100) < 20, do: 2
  def plural("mt", _), do: 3

  # Polish
  # n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2
  def plural("pl", 1),
    do: 0
  def plural("pl", n)
    when ends_in(n, [2, 3, 4]) and (rem(n, 100) < 10 or rem(n, 100) >= 20),
    do: 1
  def plural("pl", _),
    do: 2

  # Romanian
  # n==1 ? 0 : (n==0 || (n%100 > 0 && n%100 < 20)) ? 1 : 2
  def plural("ro", 1), do: 0
  def plural("ro", n) when n == 0 or (rem(n, 100) > 0 and rem(n, 100) < 20), do: 1
  def plural("ro", _), do: 2

  # Slovenian
  # n%100==1 ? 1 : n%100==2 ? 2 : n%100==3 || n%100==4 ? 3 : 0
  def plural("sl", n) when rem(n, 100) == 1, do: 1
  def plural("sl", n) when rem(n, 100) == 2, do: 2
  def plural("sl", n) when rem(n, 100) == 3, do: 3
  def plural("sl", _), do: 0

  # Match-all clause.
  def plural(locale, n) do
    recall_if_country_or_raise(locale, &plural(&1, n))
  end

  defp recall_if_country_or_raise(locale, fun) do
    case String.split(locale, "_", parts: 2, trim: true) do
      [lang, _country] -> fun.(lang)
      _other           -> raise UnknownLocaleError, locale
    end
  end
end
