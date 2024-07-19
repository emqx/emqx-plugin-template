export BUILD_WITHOUT_QUIC ?= true
export BUILD_WITHOUT_ROCKSDB ?= true

## shallow clone for speed
export REBAR_GIT_CLONE_OPTIONS += --depth=1

## Feature Used in rebar plugin emqx_plugrel
## The Feature have not enabled by default on OTP25
export ERL_FLAGS ?= -enable-feature maybe_expr

REBAR = $(CURDIR)/rebar3
SCRIPTS = $(CURDIR)/scripts

.PHONY: all
all: compile

.PHONY: ensure-rebar3
ensure-rebar3:
	@$(SCRIPTS)/ensure-rebar3.sh

$(REBAR):
	$(MAKE) ensure-rebar3

.PHONY: compile
compile: $(REBAR)
	$(REBAR) compile

.PHONY: ct
ct: $(REBAR)
	$(REBAR) as test ct -v

.PHONY: eunit
eunit: $(REBAR)
	$(REBAR) as test eunit

.PHONY: xref
xref: $(REBAR)
	$(REBAR) xref

.PHONY: cover
cover: $(REBAR)
	$(REBAR) cover

.PHONY: clean
clean: distclean

.PHONY: distclean
distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock

.PHONY: rel
rel: $(REBAR)
	$(REBAR) emqx_plugrel tar

.PHONY: fmt
fmt: $(REBAR)
	@find . \( -name '*.app.src' -o \
				-name '*.erl' -o \
				-name '*.hrl' -o \
				-name 'rebar.config' -o \
				-name '*.eterm' -o \
				-name '*.escript' \) \
				-not -path '*/_build/*' \
				-not -path '*/deps/*' \
				-not -path '*/_checkouts/*' \
				-type f \
		| xargs | $(REBAR) fmt --verbose -w