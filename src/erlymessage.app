{application, erlymessage,
 [{description, "erlymessage"},
  {vsn, "0.01"},
  {modules, [
    erlymessage,
    erlymessage_app,
    erlymessage_sup,
    erlymessage_web,
    erlymessage_deps
  ]},
  {registered, []},
  {mod, {erlymessage_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
