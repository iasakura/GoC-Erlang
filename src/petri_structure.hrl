-type domain() :: integer() | true | false | ok | q.
-record(token, {domain :: domain()}).
-type token() :: #token{}.

-record(location, {id :: string()}).
-type location() :: #location{}.

-record(address, {}).
-type address() :: #address{}.

% transitionの遷移関数を表す。
% 引数がpreにマッチしない引数に対しては例外を投げる。
% 返り値はpostにマッチする。
-type delta() :: fun((tokil() | token()) -> {ok, (tokil() | token())}).

-type porality() :: pos | neg | internal.

% transition. ∂とpre, postはここに持たせることにした。
-record(transition, {
          porality :: porality(),
          address :: address() | nil,
          pre :: [location()],
          post :: [location()],
          delta :: delta()
         }).
-type transition() :: #transition{}.

-type tokil() :: #{string() => token()}.

% deltaはpstructにおく。これはコンパイル結果でも使い回す。
-record(pstruct, {
          locations = [] :: [location()],
          transitions = [] :: [transition()]
         }).

-type pstruct() :: #pstruct{}.
