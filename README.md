
emqx-plugin-template
====================

This is a template plugin for the EMQ X broker.

TODO: Add a plugin development guide ...

Plugin Config
-------------

Each plugin should have a 'etc/{plugin_name}.conf|config' file to store application config.

Authentication and ACL
----------------------

```
emqx_access_control:register_mod(auth, ?MODULE, Env).
emqx_access_control:register_mod(acl, ?MODULE, Env).
```

Plugin and Hooks
-----------------

[Plugin Design](http://docs.emqtt.com/en/latest/design.html#plugin-design)

[Hooks Design](http://docs.emqtt.com/en/latest/design.html#hooks-design)

License
-------

Apache License Version 2.0

Author
------

EMQ X Team.
