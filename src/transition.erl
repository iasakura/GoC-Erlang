-module(transition).

-include("petri_structure.hrl").

-export([create/3]).


-spec(create(transition(), #{string() => pid()}, delta()) -> 'ok').
create(#transition{porality = Porality, address = Address, pre = Pre, post = Post}, LocationTable, Delta) ->
    ok.
