-type domain() :: integer() | true | false | ok | q.
-type token() :: domain().

-record(location, {id :: string()}).
-type location() :: #location{}.

-record(address, {}).
-type address() :: #address{}.

% transitionの遷移関数を表す。
% 引数がpreにマッチしない引数に対してはnilを返す。
% 返り値はpostにマッチする。
-type delta() :: fun((tokil()) -> {ok, tokil()} | nil).

-type porality() :: pos | neg | internal.

% transition. ∂とpre, postはここに持たせることにした。
-record(transition, {
          porality :: porality(),
          address :: address() | nil,
          pre :: [location:location()],
          post :: [location:location()],
          delta :: delta()
         }).
-type transition() :: #transition{}.

-type tokil() :: #{string() => token()}.

% deltaはpstructにおく。これはコンパイル結果でも使い回す。
-record(pstruct, {
          locations = [] :: [location:location()],
          transitions = [] :: [transition()]
         }).

-type pstruct() :: #pstruct{}.
