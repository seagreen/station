# Main Inspirations

## [Tent](https://tent.io/)

### What it Inspired

Generally: the idea of making a new protocol to compete with standard webapps.

Concretely: using immutable, JSON Schema'd data for the new system.

And simply put -- this project wouldn't exist without Daniel and Jonathan in the first place. They helped get my career going. You should check out their current company, [Flynn](https://flynn.io/).

### Differences

Station places more emphasis than Tent on separating the layers of the specification. For instance Tent posts include content, relevant information about that content, versioning info and permissions. In Station the first three of those are separated into different types of data (blobs, cards, and versions) and permissions are ignored entirely -- Station is agnostic about how you do permissioning.

While I think Station's layout gives it a solid long-term foundation, it isn't without downsides. For instance right now Station's network protocol is wildly underspecified (plain files over `git://`) which means you have to do a lot of manual work to even get two Stations communicating. Tent's network protocol is fully specified.

## [Camlistore](https://camlistore.org/)

### What it Inspired

Generally: an extreme focus on building the new datastore in layers and that the it should emphasize being useful independent of the network.

Concretely: the division of post content and metadata into separate entries in the store, so that cards link to the data they reference instead of inlining it. Also the name "blob".

### Differences

+ Camlistore describes data types using code, cards stores use a schema specification (in Station's case a modified version of JSON Schema).

+ Camlistore uses private key signing to claim ownership of data, Station uses version enumeration from your server.

+ Camlistore metadata is a summation (of claim posts referencing the data), Station metadata is a snapshot (the latest version and card).

# Other Inspirations

## Plan 9

Plan 9's [immutable foundation](https://en.wikipedia.org/wiki/Venti) and heavy emphasis on remote storage both influenced Station. Though it isn't about Plan 9 specifically, the part at the end of [this article](https://usesthis.com/interviews/rob.pike/) where Rob Pike talks about the superiority of remove over local storage for personal computing helped convince me that I wanted a single personal datastore.
