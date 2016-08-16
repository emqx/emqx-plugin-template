PROJECT = emqttd_plugin_template
PROJECT_DESCRIPTION = emqttd plugin template
PROJECT_VERSION = 2.0

DEPS = emqttd 
dep_emqttd = git https://github.com/emqtt/emqttd emq20

COVER = true

include erlang.mk

app:: rebar.config
