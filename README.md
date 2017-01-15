# Status

Extremely experimental. See [development.md](./docs/development.md) for details.

# Motivation

Websites have pages.

Webapps have posts.

Unix systems have hierarchical files.

None of these provide a solid foundation for personal computing. Let's see if we can do better.

# What's in this project?

Four things:

1. An idea: the _card_. A card is an immutable piece of data containing two [content-addressed](https://en.wikipedia.org/wiki/Content-addressable_storage) links-- one to a schema and another to a piece of data that's an instance of that schema. It may also contain other information about the instance such as its name, source, and date of creation.

  Some advantages of this design are listed [here](./docs/advantages.md).

2. A second idea: the _card store_. This is simply a datastore built on cards.

3. A specification for the first card store: _Station_.

3. A Haskell library for interacting with Station stores: `station`.

# Inspiration

Station's main inspirations are [Tent](https://tent.io/) and [Camlistore](https://camlistore.org/). A discussion of the similarities and differences between the projects is [here](./docs/inspirations.md).

# Design

Card stores are built in layers of abstraction. Higher layers know about lower layers but not vice-versa. We'll explore these layers using examples from the first card store implementation, _Station_.

## Layer 1 - Blobs

Finite sequences of 8-bit bytes.

Identified by a hash or hashes (Station uses a [base64url](https://tools.ietf.org/html/rfc4648#section-5) encoded [BLAKE2b](https://blake2.net/) hash).

Immutable (in the Haskell sense of being [inhabited by ⊥](https://en.wikipedia.org/wiki/Bottom_type), this is to reflect the reality that sometimes data is deleted locally to save space).

## Layer 2 - Cards

All cards are also blobs. Each must conform to a [hardcoded schema](schemas/human-optimized/card.json).

A card is a document that links to a blob (by hash) as well as a schema that describes the linked blob. Cards can optionally contain other data about the linked blob.

For instance, say we have this piece of JSON data (rendered with `\n` unescaped for readability):
```
{
  "stanzas": [3,4],
  "lyrics": "The well paced blips of the factory ships
             slide past our orbit's brink
             like a swarm of bees in the girder trees,
             come to our flowers to drink.

             And the earth is clean as a springtime dream,
             no factory smokes appear,
             for they've left the land to the gardener's hand,
             and they all are circling here."
}
```

Say its hash is `abc`.

Next we need a schema. The prototype of Station supports a single schema specification: a modified version of [JSON Schema](https://tools.ietf.org/html/draft-fge-json-schema-validation-00). Note that this restricts the blobs supported by the prototype implementation to JSON, as JSON Schema only works on JSON. Ways to schema other types of data will be added in future implementations.

Here's an example schema for song excerpts:
```json
{
  "type": "object",
  "required": [
    "stanzas",
    "lyrics"
  ],
  "properties": {
    "stanzas": {
      "type": "array",
      "items": {
        "type": "integer"
      },
      "minItems": 1
    },
    "lyrics": {
      "type": "string"
    }
  }
}
```

Say its hash is `xyz`.

A card to describe our specific lyric excerpt might then look like this.
```json
{
  "schema": {
    "id": "c814e306-b1ed-456e-9031-92ab16df39ed",
    "hash": {
      "blake2b-64ue": "xyz"
    }
  },
  "instance": {
    "blake2b-64ue": "abc"
  }
}
```

Cards can also contain additional information like name, original author, etc. A more developed card for our lyrics excerpt might be:
```json
{
  "name": "The Lightship excerpt",
  "authors": ["Julia Ecklar"],
  "date": "1983",
  "sources": ["Minus Ten And Counting"],
  "schema": {
    "id": "c814e306-b1ed-456e-9031-92ab16df39ed",
    "hash": {
      "blake2b-64ue": "xyz"
    }
  },
  "instance": {
    "blake2b-64ue": "abc"
  }
}
```

The ideal of cards is that two people on two different sides of the world studying the same piece of data will eventually come up with the same card. While this isn't realistic in practice, it does limit the type of information cards can contain. Cards only contain information about the data they describe, never about the card itself (such as its creator, [GUID](https://en.wikipedia.org/wiki/Globally_unique_identifier) or the date it was last modified).

## Layer 3 - Versions

All versions are also blobs. Each must conform to a [hardcoded schema](schemas/human-optimized/version.json).

A version contains information about a card itself. This includes the GUID of the card, any parent versions (directly analogous to parent commits in other VCSes), and the time the version was created. Versions measure time in [TAI](https://en.wikipedia.org/wiki/International_Atomic_Time) seconds since the Unix Epoch (TAI seconds are just normal, real life seconds-- leap seconds have no place in a standard like this one).

Let's say the hash of our card above is `defg`. Its version might then be:
```json
{
  "parents": [],
  "id": "1666ea39-1187-4b7c-9abb-e2baff3a42fa",
  "card": {
    "blake2b-64ue": "defg"
  },
  "authors": [
    {
      "id": "87e2e68b-2366-47e1-9bf2-d3c1f18d55ef",
      "card": {
        "blake2b-64ue": "hijk"
      }
    }
  ],
  "time": 1481155096.714
}
```

The `"authors"` field links to each of the creators of the card, though it will normally have a single entry. An example author card has a `"name"` field with the value `"John Smith"` and contains the data:
```json
{
  "stations": [
    "jsmith.example.com"
  ]
}
```

Note: verification of authorship is outside of the scope of the version layer. Versions with accurate and inaccurate authorship information exist side by side at this level of the system. More on this later.

One reason for keeping versions and cards at different layers is that sharing a version shares the entire Merkle DAG of your data's history (because each version links to its parents by hash). Depending on how sensitive your data is and who you're sharing it with you may not want to do this. If that's the case you can just share the latest card instead.

## Layer 4 - Version Enumeration

! MUTABILITY STARTS HERE !

An instance of a card store is called a _deck_. 

Each deck contains a content-addressable store. Everything defined in the layers below this one is just stored as entries within it. There's nothing to distinguish version blobs from non-version blobs other than their contents.

So what if someone created a blob that looked exactly like a version and listed you as an author? How would you distinguish it from things you've actually written?

One answer is to build the system so that all your posts are signed with your private key. While it's good that other projects are exploring this, Station doesn't use this method because it becomes too hard to revoke authorship. While people know everything they release onto the internet may never go away, that's doesn't mean they want to stamp it all with an unforgeable "I WROTE THIS".

Station uses a different approach. Each deck stores a set of hash links to all the versions that were created locally within it. Using Haskell types to describe it:
```haskell
type HashLink = Bytes

type VersionEnumeration = Set HashLink
```

This allows us to distinguish versions we created from the rest of the store, but how do we distinguish versions downloaded from different remote decks? It can be important to know if a version came from `closefriend.example.com` or `sketchysite.example.evil`.

The answer is that we expand our set to a map:
```haskell
type StationURI = Bytes

# This is a sum type that can be either Local or a StationURI.
data Location
    = Local
    | StationURI

type VersionEnumeration = Map Location HashLink
```

While a Station store must contain this information, at this layer of the system we don't restrict how it must be physically stored (just like we haven't specified a particular k/v store for the content-addressable store, we just require that logical relationship to exist).

Note that this is allowed to be mutable. This is because sometimes remote stores will remove versions from their enumerations, which you might then want to propagate to your store's version map (say the remote store got hacked, a bunch of false versions were added, and then the owner regained control. Or the owner simply wanted to stop claiming that they had written a version).

## Layer 5 - Ontology

An ontology is defined by the schemas within a deck.

All Stations must agree on a few schemas, the schema for schemas (seriously), the schema for authors, and the schemas for tags.

Only somewhat less important are the schemas for everything else -- blog posts, workout journals, status updates, book reviews, todo lists, etc. While two decks using totally different schemas for these are both Stations, they will still not be able to communicate much with each other. So the schemas you use are very important.

## Layer 6 - Physical Storage and Network Protocol

Physical storage is how a Station is stored on disk. This may differ between Station implementations and that's OK. It's much easier to get two datastores that agree on ontology to communicate even if one uses a flat filesystem to store things and the other uses something more structured, than it is to get two datastores to communicate that agree on physical storage but don't agree on ontology.

The network protocol is how a Station exposes itself to other programs that want to query its content-addressable store or its version enumeration. Different Stations can use different network protocols. The current implementation has no network protocol at all, it's just a flat set of files that can be made available over `git://` or `ftp://`, but this will change in the future.
