## Advantages over traditional files

+ Since cards contain schemas, a piece of data with the contents `notactually"json` can't declare itself to be a JSON document.

+ Since links to a card include its GUID, changing the name of a card won't break references to it.

+ Since links to a card include its hash, if you want to see what a the contents of a link looked like exactly when it was created, you can.

+ Since schemas are just another type of card, you can create new ones on the fly and then have them enforced.

## Advantages over webapp posts

It's actually hard to make a concrete list here since posts can actually be anything. Personally, I find it strange for the main concept in a UI not to map to any kind of concrete data structure, but maybe that's just me.
