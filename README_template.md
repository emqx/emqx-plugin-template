{{=@@ @@=}}
# @@name@@

@@description@@

## Release

An EMQX plugin release is a tar file including including a subdirectory of this plugin's name and it's version, that contains:

1. A JSON format metadata file describing the plugin
2. Versioned directories for all applications needed for this plugin (source and binaries).
3. Confirm the OTP version used by EMQX that the plugin will be installed on (See also [./.tool-versions](./.tool-versions)).

In a shell from this plugin's working directory execute `make rel` to have the package created like:

```
_build/default/emqx_plugrel/emqx_plugin_template-<vsn>.tar.gz
```
## Format

Format all the files in your project by running:
```
make fmt
```

See [EMQX documentation](https://docs.emqx.com/en/enterprise/v5.0/extensions/plugins.html) for details on how to deploy custom plugins.
