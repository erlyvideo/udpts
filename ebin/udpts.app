{application, udpts,
[{description, "udpts"},
 {vsn, "0.1"},
 {modules, [
	udpts,
	udpts_http,
	udpts_reader,
	udpts_stats,
	udpts_sup
  ]},
 {registered,[udpts,udpts_sup]},
 {applications, [kernel,stdlib]},
 {mod, {udpts,[]}}
]}.

