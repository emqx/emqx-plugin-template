# EMQX Plugin Template

This is a [rebar3 template](https://rebar3.org/docs/tutorials/templates/#custom-templates) to ease creation of EMQX Plugins in Erlang.

This plugin template is for EMQX >= 5.0.

We introduced a AVRO config schema mechanism for plugins in EMQX 5.7.0 to facilitate configuration updates for plugins at runtime via RESTAPI.
Please see [priv/config_schmea.avsc.example](./priv/config_schmea.avsc.example) and [priv/config_i18n.json.example](./priv/config_schmea.avsc.example) as examples.

For EMQX >= 4.3, please see branch emqx-v4

For older EMQX versions, plugin development is no longer maintained.

A plugin template for Elixir (experimental) can be found at https://github.com/emqx/emqx-elixir-plugin.

## Prerequisites

 + A working build environment (eg `build_essential`) including `make`
 + Erlang OTP 25 or newer recommended
 + rebar3

## Usage

```shell
$ mkdir -p ~/.config/rebar3/templates
$ pushd ~/.config/rebar3/templates
$ git clone https://github.com/emqx/emqx-plugin-template.git
$ popd
$ rebar3 new emqx-plugin my_emqx_plugin
$ make -C my_emqx_plugin rel
```

> [!NOTE]
> In order to use the AVRO config schema feature, please make sure the plugin template tag version >= 5.7.0

> [!NOTE]
> If the `REBAR_CACHE_DIR` environment variable has been set, the directory for templates should be `$REBAR_CACHE_DIR/.config/rebar3/templates`.
> [Here](https://github.com/erlang/rebar3/issues/2762) is a relevant issue.

This will create a tarball containing your custom plugin. You can use EMQX's Dashboard or it's command line tools to deploy it into your running EMQX cluster.

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.
