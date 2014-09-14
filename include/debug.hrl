%%% Asserting and validation of running code.
-ifdef(DEBUG).
-define(ASSERT(TEST),case TEST of true -> pass; _ -> error(badassert) end).
-else().
-define(ASSERT( _ ), pass).
-endif().
