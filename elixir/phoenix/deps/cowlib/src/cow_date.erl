%% Copyright (c) 2013-2014, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cow_date).

-export([rfc2109/1]).

%% @doc Return the date formatted according to RFC2109.

-spec rfc2109(calendar:datetime()) -> binary().
rfc2109({Date = {Y, Mo, D}, {H, Mi, S}}) ->
	Wday = calendar:day_of_the_week(Date),
	<<	(weekday(Wday))/binary, ", ",
		(pad_int(D))/binary, "-",
		(month(Mo))/binary, "-",
		(year(Y))/binary, " ",
		(pad_int(H))/binary, ":",
		(pad_int(Mi))/binary, ":",
		(pad_int(S))/binary, " GMT" >>.

-ifdef(TEST).
rfc2109_test_() ->
	Tests = [
		{<<"Sat, 14-May-2011 14:25:33 GMT">>, {{2011, 5, 14}, {14, 25, 33}}},
		{<<"Sun, 01-Jan-2012 00:00:00 GMT">>, {{2012, 1,  1}, { 0,  0,  0}}}
	],
	[{R, fun() -> R = rfc2109(D) end} || {R, D} <- Tests].
-endif.

-ifdef(PERF).
horse_rfc2019_20130101_000000() ->
	horse:repeat(100000,
		rfc2109({{2013, 1, 1}, {0, 0, 0}})
	).

horse_rfc2019_20131231_235959() ->
	horse:repeat(100000,
		rfc2109({{2013, 12, 31}, {23, 59, 59}})
	).

horse_rfc2019_12340506_070809() ->
	horse:repeat(100000,
		rfc2109({{1234, 5, 6}, {7, 8, 9}})
	).
-endif.

%% Internal.

-spec pad_int(0..59) -> <<_:16>>.
pad_int( 0) -> <<"00">>;
pad_int( 1) -> <<"01">>;
pad_int( 2) -> <<"02">>;
pad_int( 3) -> <<"03">>;
pad_int( 4) -> <<"04">>;
pad_int( 5) -> <<"05">>;
pad_int( 6) -> <<"06">>;
pad_int( 7) -> <<"07">>;
pad_int( 8) -> <<"08">>;
pad_int( 9) -> <<"09">>;
pad_int(10) -> <<"10">>;
pad_int(11) -> <<"11">>;
pad_int(12) -> <<"12">>;
pad_int(13) -> <<"13">>;
pad_int(14) -> <<"14">>;
pad_int(15) -> <<"15">>;
pad_int(16) -> <<"16">>;
pad_int(17) -> <<"17">>;
pad_int(18) -> <<"18">>;
pad_int(19) -> <<"19">>;
pad_int(20) -> <<"20">>;
pad_int(21) -> <<"21">>;
pad_int(22) -> <<"22">>;
pad_int(23) -> <<"23">>;
pad_int(24) -> <<"24">>;
pad_int(25) -> <<"25">>;
pad_int(26) -> <<"26">>;
pad_int(27) -> <<"27">>;
pad_int(28) -> <<"28">>;
pad_int(29) -> <<"29">>;
pad_int(30) -> <<"30">>;
pad_int(31) -> <<"31">>;
pad_int(32) -> <<"32">>;
pad_int(33) -> <<"33">>;
pad_int(34) -> <<"34">>;
pad_int(35) -> <<"35">>;
pad_int(36) -> <<"36">>;
pad_int(37) -> <<"37">>;
pad_int(38) -> <<"38">>;
pad_int(39) -> <<"39">>;
pad_int(40) -> <<"40">>;
pad_int(41) -> <<"41">>;
pad_int(42) -> <<"42">>;
pad_int(43) -> <<"43">>;
pad_int(44) -> <<"44">>;
pad_int(45) -> <<"45">>;
pad_int(46) -> <<"46">>;
pad_int(47) -> <<"47">>;
pad_int(48) -> <<"48">>;
pad_int(49) -> <<"49">>;
pad_int(50) -> <<"50">>;
pad_int(51) -> <<"51">>;
pad_int(52) -> <<"52">>;
pad_int(53) -> <<"53">>;
pad_int(54) -> <<"54">>;
pad_int(55) -> <<"55">>;
pad_int(56) -> <<"56">>;
pad_int(57) -> <<"57">>;
pad_int(58) -> <<"58">>;
pad_int(59) -> <<"59">>.

-spec weekday(1..7) -> <<_:24>>.
weekday(1) -> <<"Mon">>;
weekday(2) -> <<"Tue">>;
weekday(3) -> <<"Wed">>;
weekday(4) -> <<"Thu">>;
weekday(5) -> <<"Fri">>;
weekday(6) -> <<"Sat">>;
weekday(7) -> <<"Sun">>.

-spec month(1..12) -> <<_:24>>.
month( 1) -> <<"Jan">>;
month( 2) -> <<"Feb">>;
month( 3) -> <<"Mar">>;
month( 4) -> <<"Apr">>;
month( 5) -> <<"May">>;
month( 6) -> <<"Jun">>;
month( 7) -> <<"Jul">>;
month( 8) -> <<"Aug">>;
month( 9) -> <<"Sep">>;
month(10) -> <<"Oct">>;
month(11) -> <<"Nov">>;
month(12) -> <<"Dec">>.

-spec year(pos_integer()) -> <<_:32>>.
year(1970) -> <<"1970">>;
year(1971) -> <<"1971">>;
year(1972) -> <<"1972">>;
year(1973) -> <<"1973">>;
year(1974) -> <<"1974">>;
year(1975) -> <<"1975">>;
year(1976) -> <<"1976">>;
year(1977) -> <<"1977">>;
year(1978) -> <<"1978">>;
year(1979) -> <<"1979">>;
year(1980) -> <<"1980">>;
year(1981) -> <<"1981">>;
year(1982) -> <<"1982">>;
year(1983) -> <<"1983">>;
year(1984) -> <<"1984">>;
year(1985) -> <<"1985">>;
year(1986) -> <<"1986">>;
year(1987) -> <<"1987">>;
year(1988) -> <<"1988">>;
year(1989) -> <<"1989">>;
year(1990) -> <<"1990">>;
year(1991) -> <<"1991">>;
year(1992) -> <<"1992">>;
year(1993) -> <<"1993">>;
year(1994) -> <<"1994">>;
year(1995) -> <<"1995">>;
year(1996) -> <<"1996">>;
year(1997) -> <<"1997">>;
year(1998) -> <<"1998">>;
year(1999) -> <<"1999">>;
year(2000) -> <<"2000">>;
year(2001) -> <<"2001">>;
year(2002) -> <<"2002">>;
year(2003) -> <<"2003">>;
year(2004) -> <<"2004">>;
year(2005) -> <<"2005">>;
year(2006) -> <<"2006">>;
year(2007) -> <<"2007">>;
year(2008) -> <<"2008">>;
year(2009) -> <<"2009">>;
year(2010) -> <<"2010">>;
year(2011) -> <<"2011">>;
year(2012) -> <<"2012">>;
year(2013) -> <<"2013">>;
year(2014) -> <<"2014">>;
year(2015) -> <<"2015">>;
year(2016) -> <<"2016">>;
year(2017) -> <<"2017">>;
year(2018) -> <<"2018">>;
year(2019) -> <<"2019">>;
year(2020) -> <<"2020">>;
year(2021) -> <<"2021">>;
year(2022) -> <<"2022">>;
year(2023) -> <<"2023">>;
year(2024) -> <<"2024">>;
year(2025) -> <<"2025">>;
year(2026) -> <<"2026">>;
year(2027) -> <<"2027">>;
year(2028) -> <<"2028">>;
year(2029) -> <<"2029">>;
year(Year) -> list_to_binary(integer_to_list(Year)).
