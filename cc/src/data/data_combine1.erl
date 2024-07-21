-module(data_combine1).
-export([get/1]).
-include("combine1.hrl").
get(170001) -> #combine1{num=3, gold=1000, tid=170002};
get(170002) -> #combine1{num=3, gold=2000, tid=170003};
get(170003) -> #combine1{num=3, gold=3000, tid=170004};
get(_) -> undefined.