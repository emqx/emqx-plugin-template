PROJECT = emqttd_plugin_template
PROJECT_DESCRIPTION = emqttd plugin template
PROJECT_VERSION = 1.1

DEPS = emqttd 
dep_emqttd = git https://github.com/emqtt/emqttd plus

include erlang.mk

app:: rebar.config
