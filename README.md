# Corydalis

… is a simple application to help manage offline (as in non-cloud)
collections of RAW and processed pictures. It also can serve as an
image viewer.

Is it for you? If you need/could use:

* help in understanding a large collection of pictures
* an image viewer for browsing the pictures stored on your NAS from
  various web and mobile clients

Then maybe yes. Although this is very much a work in progress, and has
very sharp edges.

On the other hand, if you never shoot RAW, or if all the pictures you
take are stored in the cloud, then this is probably/most likely not
for you. I personally prefer to manage and be able to browse my
pictures without internet access - the entire web interface (fonts,
CSS, etc. is served locally, without relying on CDNs).

## What it doesn't do

It doesn't actually modify your collection in any way. In other words,
it's not a "collection manager" (the way e.g. Adobe Lightroom is), but
rather is designed to work in conjunction with an actual manager.

## Installation and use

You will need [stack](https://docs.haskellstack.org/en/stable/README/)
and to read the [installation guide](docs/install.md). Then read the
[manual](docs/manual.md) in order to understand what Corydalis shows.

## Bugs/features

There is a raw list of ideas/issues in the [todo
list](docs/todo.md). Otherwise, feel free to file bugs.
