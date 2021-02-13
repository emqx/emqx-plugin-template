{application,emqx_plugin_template,
  [{description,"EMQ X Plugin Template"},
    {vsn,"4.2.3"},
    {modules,[emqx_plugin_template,emqx_plugin_template_app,
      emqx_plugin_template_cli,emqx_plugin_template_sub,
      emqx_plugin_template_sup]},
    {registered,[emqx_plugin_template_sup]},
    {applications,[kernel,stdlib,eredis,eredis_cluster,ecpool]},
    {mod,{emqx_plugin_template_app,[]}}]}.
