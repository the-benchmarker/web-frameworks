%% Copyright (c) 2014, Loïc Hoguin <essen@ninenines.eu>
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

-ifndef(COW_INLINE_HRL).
-define(COW_INLINE_HRL, 1).

%% INLINE_LOWERCASE(Function, Rest, Acc, ...)
%%
%% To be included at the end of a case block.
%% Defined for up to 10 extra arguments.

-define(INLINE_LOWERCASE(Function, Rest, Acc),
	$A -> Function(Rest, << Acc/binary, $a >>);
	$B -> Function(Rest, << Acc/binary, $b >>);
	$C -> Function(Rest, << Acc/binary, $c >>);
	$D -> Function(Rest, << Acc/binary, $d >>);
	$E -> Function(Rest, << Acc/binary, $e >>);
	$F -> Function(Rest, << Acc/binary, $f >>);
	$G -> Function(Rest, << Acc/binary, $g >>);
	$H -> Function(Rest, << Acc/binary, $h >>);
	$I -> Function(Rest, << Acc/binary, $i >>);
	$J -> Function(Rest, << Acc/binary, $j >>);
	$K -> Function(Rest, << Acc/binary, $k >>);
	$L -> Function(Rest, << Acc/binary, $l >>);
	$M -> Function(Rest, << Acc/binary, $m >>);
	$N -> Function(Rest, << Acc/binary, $n >>);
	$O -> Function(Rest, << Acc/binary, $o >>);
	$P -> Function(Rest, << Acc/binary, $p >>);
	$Q -> Function(Rest, << Acc/binary, $q >>);
	$R -> Function(Rest, << Acc/binary, $r >>);
	$S -> Function(Rest, << Acc/binary, $s >>);
	$T -> Function(Rest, << Acc/binary, $t >>);
	$U -> Function(Rest, << Acc/binary, $u >>);
	$V -> Function(Rest, << Acc/binary, $v >>);
	$W -> Function(Rest, << Acc/binary, $w >>);
	$X -> Function(Rest, << Acc/binary, $x >>);
	$Y -> Function(Rest, << Acc/binary, $y >>);
	$Z -> Function(Rest, << Acc/binary, $z >>);
	C -> Function(Rest, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, Acc),
	$A -> Function(Rest, A0, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, << Acc/binary, $z >>);
	C -> Function(Rest, A0, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, Acc),
	$A -> Function(Rest, A0, A1, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, Acc),
	$A -> Function(Rest, A0, A1, A2, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, A5, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, A5, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, A5, A6, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, A5, A6, A7, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, << Acc/binary, C >>)
).

-define(INLINE_LOWERCASE(Function, Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, Acc),
	$A -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $a >>);
	$B -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $b >>);
	$C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $c >>);
	$D -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $d >>);
	$E -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $e >>);
	$F -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $f >>);
	$G -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $g >>);
	$H -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $h >>);
	$I -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $i >>);
	$J -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $j >>);
	$K -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $k >>);
	$L -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $l >>);
	$M -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $m >>);
	$N -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $n >>);
	$O -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $o >>);
	$P -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $p >>);
	$Q -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $q >>);
	$R -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $r >>);
	$S -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $s >>);
	$T -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $t >>);
	$U -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $u >>);
	$V -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $v >>);
	$W -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $w >>);
	$X -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $x >>);
	$Y -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $y >>);
	$Z -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, $z >>);
	C -> Function(Rest, A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, << Acc/binary, C >>)
).

%% INLINE_LOWERCASE_BC(Bin)
%%
%% Lowercase the entire binary string in a binary comprehension.

-define(INLINE_LOWERCASE_BC(Bin),
	<< << case C of
		$A -> $a;
		$B -> $b;
		$C -> $c;
		$D -> $d;
		$E -> $e;
		$F -> $f;
		$G -> $g;
		$H -> $h;
		$I -> $i;
		$J -> $j;
		$K -> $k;
		$L -> $l;
		$M -> $m;
		$N -> $n;
		$O -> $o;
		$P -> $p;
		$Q -> $q;
		$R -> $r;
		$S -> $s;
		$T -> $t;
		$U -> $u;
		$V -> $v;
		$W -> $w;
		$X -> $x;
		$Y -> $y;
		$Z -> $z;
		C -> C
	end >> || << C >> <= Bin >>).

-endif.
