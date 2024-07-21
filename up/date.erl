-module(date).
-export([convert/1]).

convert(Day) ->
	case Day of
		1 -> monday;
		2 -> tuesday;
		3 -> wednesday;
		4 -> thursday;
		5 -> friday;
		6 -> saturday;
		7 -> sunday;
		Other -> {error,unknown_shape,Other}
	end.