% local gproc
-define(GPROC_KEY(Name), {n, l, Name}).
-define(GET_PID(Name), gproc:where(?GPROC_KEY(Name))).
-define(REG_PID(Name), gproc:reg(?GPROC_KEY(Name))). %% Only can reg yourself.
-define(UNREG(Name), gproc:unreg(?GPROC_KEY(Name))).
-define(SUBSCRIBE(Channel), gproc:reg({p, l, Channel})).
-define(UNSUBSCRIBE(Channel), gproc:unreg({p, l, Channel})).
-define(PUBLISH(Channel, Msg), gproc:send({p, l, Channel}, Msg)).

% Broadcast Channel Msg
-define(BROADCAST(Channel, MsgType, Msg), ?PUBLISH(Channel, {gproc_msg, MsgType, Msg})).

