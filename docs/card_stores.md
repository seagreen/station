## Definition

Every card store contains blobs, cards and versions. Cards must specify a schema blob and a blob that's an instance of that schema. Versions must specify a card blob (or the deletion of one), the ID of that card, and the version's parents.

Each card store also defines:

+ A set of hashes to identify blobs. Station currently uses { (base64-encoded blake2b) }.

+ A set of schemas. Station currently uses { ([modified](./schema.md) JSON Schema) }.

+ A hardcoded schema for cards.

+ A hardcoded schema for versions.

If two implementations' definitions of these attributes overlap then they're describing the same card store.

## Note

This doesn't include things like physical storage medium.

Getting two stores that disagree on physical storage but agree on ontology to communicate can be done with little trouble.

Getting two stores that agree on physical storage but disagree on ontology to communicate can be extremely difficult.
