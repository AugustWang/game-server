-module(mp3_sync).
-export([find_sync/2,is_header/2]).
-compile(export_all).

find_sync(Bin, N) ->
    case is_header(N, Bin) of
	{ok, Len1, _} ->
	    case is_header(N + Len1, Bin) of
		{ok, Len2, _} ->
		    case is_header(N + Len1 + Len2, Bin) of
			{ok, _, _} ->
			    {ok, N};
			error ->
			    find_sync(Bin, N+1)
		    end;
		error ->
		    find_sync(Bin, N+1)
	    end;
	error ->
	    find_sync(Bin, N+1)
    end.

is_header(N,Bin) ->
	unpack_header(get_word(N,Bin)).

get_word(N,Bin) ->
	{_,<<C:4/binary,_/binary>>} = split_binary(Bin,N),
	C.

unpack_header(X) ->
	try decode_header(X)
	catch
		_:_ -> error
	end.