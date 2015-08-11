
## emqttd plugin template

This is a template plugin for emqttd project.


## Plugin Config

Each plugin should have a 'etc/plugin.config' file to store application config.


## Authentication and ACL

```
emqttd_access_control:register_mod(auth, ?MODULE, Env).
emqttd_access_control:register_mod(acl, ?MODULE, Env).
```


## Plugin and Hooks

[Plugin Design](https://github.com/emqtt/emqttd/wiki/Plugin%20Design)

[Hooks Design](https://github.com/emqtt/emqttd/wiki/Hooks%20Design)


