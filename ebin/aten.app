{application, 'aten', [
	{description, "Node failure detector"},
	{vsn, "0.1.0"},
	{modules, ['aten','aten_app','aten_detect','aten_detector','aten_emitter','aten_sink','aten_sup']},
	{registered, [aten_sup]},
	{applications, [kernel,stdlib,sasl,crypto]},
	{mod, {aten_app, []}},
	{env, [
	{poll_interval, 1000},
	{heartbeat_interval, 100},
	{detection_threshold, 0.99}
]}
]}.