{application, udpts,
[{description, "udpts"},
 {vsn, "0.1"},
 {modules, [
	udpts,
	udpts_sup
  ]},
 {registered,[udpts,udpts_sup]},
 {applications, [kernel,stdlib]},
 {mod, {udpts,[]}}
]}.

