## to build emqtt without QUIC
export BUILD_WITHOUT_QUIC = 1

## Feature Used in rebar plugin emqx_plugrel
## The Feature have not enabled by default on OTP25
export ERL_FLAGS ?= -enable-feature maybe_expr
export DOCKER_COMPOSE_FILE = $(CURDIR)/.ci/docker-compose.yml

REBAR = $(CURDIR)/rebar3
SCRIPTS = $(CURDIR)/scripts

TEST_ASSETS_DIR = $(CURDIR)/_build/test/lib/emqx_pt/test/assets

.PHONY: all
all: compile

.PHONY: ensure-rebar3
ensure-rebar3:
	@$(SCRIPTS)/ensure-rebar3.sh

$(REBAR):
	$(MAKE) ensure-rebar3

.PHONY: ct
ct: $(REBAR)
	$(REBAR) as test ct -v --readable=true

.PHONY: clean
clean: clean
	@rm -rf _build
	@rm -f rebar.lock

.PHONY: install-rebar-template
install-rebar-template:
	$(SCRIPTS)/install-rebar-template.sh

.PHONY: build-test-plugins
build-test-plugins: $(REBAR)
	$(SCRIPTS)/build-sample-plugin.sh --tag 1.0.0 --name my_emqx_plugin_avsc --with-avsc --output-dir $(TEST_ASSETS_DIR)
	$(SCRIPTS)/build-sample-plugin.sh --tag 1.0.0 --name my_emqx_plugin --output-dir $(TEST_ASSETS_DIR)

.PHONY: fmt
fmt: $(REBAR)
	$(REBAR) fmt --verbose -w

.PHONY: fmt-check
fmt-check: $(REBAR)
	$(REBAR) fmt --verbose --check

.PHONY: fmt-template
fmt-template: $(REBAR)
	./scripts/format-template-code.sh

.PHONY: fmt-template-check
fmt-template-check: $(REBAR)
	./scripts/format-template-code.sh --check

.PHONY: up
up:
	docker compose -f .ci/docker-compose.yml up --detach --build --force-recreate

.PHONY: down
down:
	docker compose -f .ci/docker-compose.yml down --volumes

