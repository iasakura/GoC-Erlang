-type domain() :: integer() | 'true' | 'false' | 'ok' | 'Q'.
-type token() :: domain().

-record(location, {id :: string()}).
-type location() :: #location{}.

-record(address, {}).
-type address() :: #address{}.

% transitionの遷移関数を表す。
% 引数がpreにマッチしない引数に対しては'nil'を返す。
% 返り値はpostにマッチする。
-type delta() :: fun(([tokil()]) -> [tokil()] | 'nil').

% transition. ∂とpre, postはここに持たせることにした。
-record(transition, {
          porality :: 'pos' | 'neg' | internal,
          address :: address() | 'nil',
          pre :: [location:location()],
          post :: [location:location()],
          delta :: delta()
         }).
-type transition() :: #transition{}.

-record(tokil, {
          location :: location:location(),
          token :: token()
         }).
-type tokil() :: #tokil{}.

% deltaはpstructにおく。これはコンパイル結果でも使い回す。
-record(pstruct, {
          locations = [] :: [location:location()],
          transitions = [] :: [transition()]
         }).

-type pstruct() :: #pstruct{}.
