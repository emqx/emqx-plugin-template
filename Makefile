## shallow clone for speed

BUILD_WITHOUT_QUIC ?= true
export BUILD_WITHOUT_QUIC

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

xref:
	$(REBAR) xref

cover:
	$(REBAR) cover

clean: distclean

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

plugin-zip:
	$(REBAR) as emqx_plugin_rel compile
	echo a
