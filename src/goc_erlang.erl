%% Geometry of Causality compiler to Erlang processes.
-module(goc_erlang).

-export([compile/1]).

-type domain() :: integer() | 'true' | 'false' | 'ok' | 'Q'.
-type token() :: domain().

-record(location, {}).
-type location() :: #location{}.

-record(address, {}).
-type address() :: #address{}.

% transition. ∂とpre, postはここに持たせることにした。
-record(transition, {
          porality :: 'pos' | 'neg',
          address :: address() | 'nil',
          pre :: [location()],
          post :: [location()]
         }).
-type transition() :: #transition{}.

-record(tokil, {
          location :: location(),
          token :: token()
         }).
-type tokil() :: #tokil{}.

% deltaはpstructにおく。これはコンパイル結果でも使い回す。
-record(pstruct, {
          locations = [] :: [location()],
          transitions = [] :: [transition()],
          % 引数がpreにマッチしない引数に対しては'nil'を返す。
          % 返り値はpostにマッチする。
          delta :: fun((transition(), [tokil()]) -> [tokil()] | 'nil')
         }).

-type pstruct() :: #pstruct{}.


-spec compile(pstruct()) -> 'ok'.
compile(2) -> ng.
