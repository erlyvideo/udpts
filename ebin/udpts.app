{application, udpts,
[{description, "udpts"},
 {vsn, "0.1"},
 {modules, [
   ems_network_lag_monitor,
   gen_listener,
   udpts,
   udpts_sup,
   udpts_http,
   udpts_reader,
   udpts_stats,
   udpts_sup
 ]},
 {registered,[udpts,udpts_sup]},
 {applications, [kernel,stdlib]},
 {mod, {udpts,[]}}
]}.

