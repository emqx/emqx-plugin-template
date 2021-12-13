# emqx-plugin-template

This is a template plugin for EMQ X version >= 4.3.

For development guide, see https://github.com/emqx/emqx/blob/main-v4.4/lib-extra/README.md

Plugin Config
-------------

Each plugin should have a 'etc/{plugin_name}.conf|config' file to store application config.

Authentication and ACL
----------------------

```
emqx:hook('client.authenticate', fun ?MODULE:on_client_authenticate/3, [Env]).
emqx:hook('client.check_acl', fun ?MODULE:on_client_check_acl/5, [Env]).
```

Plugin and Hooks
-----------------

[Plugin Development](https://docs.emqx.io/en/broker/v4.3/advanced/plugins.html#plugin-development)

[EMQ X Hook points](https://docs.emqx.io/en/broker/v4.3/advanced/hooks.html)

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.
