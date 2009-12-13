{application, yarmo,
 [{description, "yarmo"},
  {vsn, "0.01"},
  {modules, [
    yarmo,
    yarmo_app,
    yarmo_sup,
    yarmo_web,
    yarmo_deps
  ]},
  {registered, []},
  {mod, {yarmo_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
