{application, erlymsg,
 [{description, "erlymsg"},
  {vsn, "0.01"},
  {modules, [
    erlymsg,
    erlymsg_app,
    erlymsg_sup,
    erlymsg_web,
    erlymsg_deps
  ]},
  {registered, []},
  {mod, {erlymsg_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
