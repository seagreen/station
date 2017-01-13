

TODO: Under construction


# With Card Stores

+ Won't work with massive datasets like physics simulation data that you want to be mutable. This is OK though, card stores aren't trying to do everything.

# With Station (temporary)

+ Wildly inefficient disk space use. Since physical storage can be tackled independently of the rest of the standard I haven't worked on making it efficient at all. If you put big bytestrings in Station and then change them a lot you'll use an absurd amount of space.

+ A total lack of a network protocol, meaning there's no way to share only some posts with someone without doing manual work (e.g. by writing a script to dump some posts into a Dropbox folder shared with them).

# With Station (long term)

+ JSON isn't a perfect data format.
