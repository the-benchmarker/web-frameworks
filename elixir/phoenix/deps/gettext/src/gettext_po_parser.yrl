Nonterminals grammar translations translation pluralizations pluralization
             strings comments maybe_msgctxt.
Terminals str msgid msgid_plural msgctxt msgstr plural_form comment.
Rootsymbol grammar.

grammar ->
  translations : '$1'.

% A series of translations. It can be just comments (which are discarded and can
% be empty anyways) or comments followed by a translation followed by other
% translations; in the latter case, comments are attached to the translation
% that follows them.
translations ->
  comments : [].
translations ->
  comments translation translations : [add_comments_to_translation('$2', '$1')|'$3'].

translation ->
  maybe_msgctxt msgid strings msgstr strings : {translation, #{
    comments       => [],
    msgid          => '$3',
    msgstr         => '$5',
    po_source_line => extract_line('$2')
  }}.
translation ->
  maybe_msgctxt msgid strings msgid_plural strings pluralizations : {plural_translation, #{
    comments       => [],
    msgid          => '$3',
    msgid_plural   => '$5',
    msgstr         => plural_forms_map_from_list('$6'),
    po_source_line => extract_line('$2')
  }}.

pluralizations ->
  pluralization : ['$1'].
pluralizations ->
  pluralization pluralizations : ['$1'|'$2'].

pluralization ->
  msgstr plural_form strings : {'$2', '$3'}.

strings ->
  str : [extract_simple_token('$1')].
strings ->
  str strings : [extract_simple_token('$1')|'$2'].

comments ->
  '$empty' : [].
comments ->
  comment comments : [extract_simple_token('$1')|'$2'].

%% For now, we ignore the msgctxt.
maybe_msgctxt ->
  '$empty' : [].
maybe_msgctxt ->
  msgctxt strings : [].

Erlang code.

extract_simple_token({_Token, _Line, Value}) ->
  Value.

extract_line({_Token, Line}) ->
  Line.

plural_forms_map_from_list(Pluralizations) ->
  Tuples = lists:map(fun extract_plural_form/1, Pluralizations),
  maps:from_list(Tuples).

extract_plural_form({{plural_form, _Line, PluralForm}, String}) ->
  {PluralForm, String}.

add_comments_to_translation({TranslationType, Translation}, Comments) ->
  {TranslationType, maps:put(comments, Comments, Translation)}.
