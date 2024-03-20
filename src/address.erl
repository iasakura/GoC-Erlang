-module(address).

-include("petri_structure.hrl").

-export([porality/1]).


-spec(negate(pos | neg) -> neg | pos).
negate(pos) -> neg;
negate(neg) -> pos.


-spec(porality(address()) -> pos | neg).
porality(q) -> neg;
porality(a) -> pos;
porality({turnstile, left, M}) -> negate(porality(M));
porality({imply, right, M}) -> negate(porality(M));
porality({_, _, M}) -> porality(M).
