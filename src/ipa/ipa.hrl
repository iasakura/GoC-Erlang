-type wo_type() ::
        ipa_unit | ipa_bool | ipa_nat | {ipa_fun, ipa_type(), wo_type()}.

-type ipa_type() :: wo_type() | ref | semaphores.

-type ipa_data() :: unit | boolean() | integer().

-type udf() :: fun((ipa_data(), ipa_data()) -> ipa_data()).

-type ipa_term() ::
        skip |
        tt |
        ff |
        {nat, integer()} |
        {var, string()} |
        {lam, string(), ipa_type(), ipa_term()} |
        {y, ipa_term()} |
        {app, ipa_term(), ipa_term()} |
        {grab, ipa_term()} |
        {ifb, ipa_term(), ipa_term(), ipa_term()} |
        {rel, ipa_term()} |
        {call, udf(), ipa_term(), ipa_term()} |
        {par, ipa_term(), ipa_term()} |
        {'let', string(), ipa_term(), ipa_term()} |
        {newref, string(), ipa_term()} |
        {assign, ipa_term(), ipa_term()} |
        {deref, ipa_term()} |
        {newsem, string(), ipa_term()}.
