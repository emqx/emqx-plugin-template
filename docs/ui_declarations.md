# Declarative UI Description

## Overview

The Declarative UI Description is an optional feature that allows plugins to provide a declarative description of the UI components for their configuration. This feature enables the EMQX Dashboard to dynamically generate configuration forms, making it easier to configure and manage plugins.

## Configuration Item Descriptions

- `component`<br />
  Required. Specifies the component type for displaying and configuring data of different values and types. Supported components include:

  | Component Name     | Description                                                  |
  | :----------------- | :----------------------------------------------------------- |
  | `input`            | Text input box for short texts or strings                    |
  | `input-password`   | Password input box that conceals input                       |
  | `input-number`     | Numeric input box allowing only numeric input                |
  | `input-textarea`   | Text area for longer text entries                            |
  | `input-array`      | Array input box for comma-separated values, supporting string and numeric arrays |
  | `switch`           | Toggle switch for boolean values                             |
  | `select`           | Dropdown selection box for enumerated types                  |
  | `code-editor`      | Code editor for specific formats (e.g., SQL, JSON)           |
  | `key-value-editor` | Editor for editing key-value pairs in Avro maps              |
  | `maps-editor`      | Editor for editing object arrays in Avro objects             |
- `label`<br />
  Required. Defines the field's label or name, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.
- `description`<br />
  Optional. Provides a detailed description of the field, supports `$msgid` for internationalization. If i18n is not configured, the original text will be displayed directly.
- `flex`<br />
  Required. Defines the proportion of the field in the grid layout; a full grid (24) spans an entire row, while a half grid (12) covers half a row.
- `required`<br />
  Optional. Indicates whether the field is mandatory.
- `format` (Applicable only for `code-editor` component)<br />
  Optional. Specifies the supported data formats, such as `sql` or `json`.
- `options` (Applicable only for `select` component)<br />
  Optional. Lists the selectable options, aligned with the symbols in the Avro Schema. Example:

  ```json
  [
    {
      "label": "$mysql",
      "value": "MySQL"
    },
    {
      "label": "$pgsql",
      "value": "postgreSQL"
    }
  ]
  ```
- `items` (Applicable only for maps-editor component)<br />
  Optional. When using the maps-editor component, specify the field name and description of the items in the form. For example:

  ```json
  {
    "items": {
      "optionName": {
        "label": "$optionNameLabel",
        "description": "$optionDesc",
        "type": "string"
      },
      "optionValue": {
        "label": "$optionValueLabel",
        "description": "$optionValueDesc",
        "type": "string"
      }
    }
  }
  ```
- `rules`<br />
  Optional. Defines validation rules for the field, where multiple rules can be configured. Supported types include:

  - `pattern`: Requires a regular expression for validation.
  - `range`: Validates numeric input within a specified range. This validation can be configured with both a minimum value (`min`) and a maximum value (`max`), which can be set either together or independently.
  - `length`: Validates the character count of input, ensuring it falls within a specified range. This validation rule allows for the configuration of both a minimum length (`minLength`) and a maximum length (`maxLength`), which can be set either together or individually.
  - `message`: Specifies an error message to display when validation fails. This supports internationalization using `$msgid` to accommodate multiple languages.

### Example Validation Rules

The following are several example snippets. For more detailed examples, refer to `priv/config_schema.avsc.example`:

```json
{
    "rules": [
    {
      "type": "pattern",
      "pattern": "^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$",
      "message": "$hostname_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "range",
      "min": 1,
      "max": 65535,
      "message": "$port_range_validate"
    }
  ]
}
```

```json
{
    "rules": [
    {
      "type": "length",
      "minLength": 8,
      "maxLength": 128,
      "message": "$password_length_validate"
    },
    {
      "type": "pattern",
      "pattern": "^(?=.*[a-z])(?=.*[A-Z])(?=.*\d)[a-zA-Z\d]*$",
      "message": "$password_validate"
    }
  ]
}
```

## Localization

There is also an **optional** internationalization (i18n) config file, located at `priv/config_i18n.json`. This file is structured as key-value pairs, for example: `{ "$msgid": { "zh": "消息", "en": "Message" } }`.
To support multiple languages in field names, descriptions, validation rule messages, and other UI elements in the `$ui` configuration, use `$msgid` prefixed with `$` in the relevant UI configurations.

