PROJECT = emq_plugin_template
PROJECT_DESCRIPTION = EMQ Plugin Template
PROJECT_VERSION = 2.1

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_plugin_template.conf -i priv/emq_plugin_template.schema -d data
