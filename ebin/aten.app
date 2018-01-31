{application, 'aten', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['aten','aten_app','aten_detect','aten_detector','aten_emitter','aten_sink','aten_sup']},
	{registered, [aten_sup]},
	{applications, [kernel,stdlib]},
	{mod, {aten_app, []}},
	{env, []}
]}.