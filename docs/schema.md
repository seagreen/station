## Info on standard JSON Schema Draft 4

Currently the most used JSON Schema spec.

Core: [draft-zyp-json-schema-04](https://tools.ietf.org/html/draft-zyp-json-schema-04).

Validation: [draft-fge-json-schema-validation-00](https://tools.ietf.org/html/draft-fge-json-schema-validation-00).

## Info on Station's modifications

Described in code [here](../src/Station/Schema/Validator/).

### "$ref"

Restrict the resolution of the validator. Links to external schemas must to use the form: `card://?id=<id>version=blake2b-64ue:<version_hash>`. This has the advantage that all schema references are now immutable.

### "hashOf"

A new validator.

Takes a subschema as its value. The data being validated must have a content-addressed link at this location, and the contents of that links must be valid against the subschema.

The content-addressed link can be any kind of JSON value.

Similar to `"$ref"`, Station's support of "hashOf" is restricted. At the moment links must be of the form:
```
{
  "blake2b-64ue": "6sZrwr61VVVdqvioSeQQpLzWIveeyDk3grCiv10ge6I4wU3Mxp7_aVHvDdcpfEBCusgu5GiQEnuI4L65w5Cx9w"
}
```

`64ue` is an abbreviation for Base64 URL-encoded. `ue` is used instead of `url` because in other situations these links are serialized to URLs. When this happens having `url` partway through a URL looks confusing.

So the above data would be valid against the following schema as long as `"6sZrwr..."` resolves to a JSON number:
```
{
  "hashOf":
    {
      "type": "number"
    }
}
```

### "linkTo"

A new validator.

Same as "hashOf", except that the content-addressable link must include an ID of some kind. It can also require interpretation and multiple lookups to be fully resolved.

An example link going directly to a piece of data:
```
{
  "id": "7f353b01-7c7a-419c-a026-548b20394031",
  "blob":
    {
      "blake2b-64ue": "9W1glLd04HDurDkySkwBCcJ234Cht2ZiqEoDiDBQpH_gxAxLuXQvFO7lnBb4ZS6JtGt6Del4_m05f6ixHpXE5A"
    }
}
```

An example link going to a version, which to be resolved will required looking up the associated card and instance:
```
{
  "id": "30117ade-ed3a-4b68-9991-1919b9c9d347",
  "version":
    {
      "blake2b-64ue": "6sZrwr61VVVdqvioSeQQpLzWIveeyDk3grCiv10ge6I4wU3Mxp7_aVHvDdcpfEBCusgu5GiQEnuI4L65w5Cx9w"
    }
}

```
