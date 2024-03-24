-type name() :: string().

-type operation() :: 'tensor' | 'turnstile' | 'imply'.
-type address() ::
        {operation(), 'left' | 'right', address()} |
        {'variable', name(), address()} |
        {reference, 'write' | 'read', address()} |
        {semaphore, grab | release, address()} |
        q |
        a.

-type domain() :: integer() | boolean() | ok | q.
-record(token, {domain :: domain()}).
-type token() :: #token{}.

-type loc_id() :: string() | [integer() | loc_id()].

-record(location, {id :: loc_id()}).
-type location() :: #location{}.

% transitionの遷移関数を表す。
% 引数がpreにマッチしない引数に対しては例外を投げる。
% 返り値はpostにマッチする。
-type delta() :: fun((tokil() | token()) -> {ok, (tokil() | token())} | nil).

-type porality() :: pos | neg | internal.

% transition. ∂とpre, postはここに持たせることにした。
-record(transition, {
          address :: address() | nil,
          pre :: [location()],
          post :: [location()],
          delta :: delta()
         }).
-type transition() :: #transition{}.

-type tokil() :: #{location() => token()}.

% deltaはpstructにおく。これはコンパイル結果でも使い回す。
-record(pstruct, {
          locations = [] :: [location()],
          transitions = [] :: [transition()]
         }).

-type pstruct() :: #pstruct{}.
