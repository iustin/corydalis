# Corydalis

… is a simple application to help manage offline (as in non-cloud)
collections of RAW and processed pictures. It doesn't replace your RAW
processor (e.g. Adobe Lightroom, DxO PhotoLab, Capture one, Darktable,
etc.) but instead acts as a companion to it, giving a parallel view on
all your files.

Corydalis is also an image and movie viewer, designed to work across
and understand your entire photo collection; the desktop-only viewers
(e.g. Geeqie) are folder-based, not collection-based. This aspect aims
to offer a similar experience of viewing your photos and movies as it
would be if you stored them in the cloud (but many features are still
missing).

Is it for you? If you need/could use:

* help in understanding a large collection of pictures, or
* an image/movie viewer for browsing the pictures stored on your NAS
  from various web and mobile clients,

Then maybe yes. Although this is very much a work in progress, and has
very sharp edges, I use it to analyse my picture collection since
~2013 and as my main image viewer since mid-2017.

On the other hand, if you never shoot RAW, or if all the pictures you
take are stored in the cloud, then this is probably/most likely not
for you. I personally prefer to manage and be able to browse my
pictures without internet access - the entire web interface (fonts,
CSS, etc. is served locally, without relying on CDNs).

## Demo site

There is a demo site at
[demo.corydalis.io](https://demo.corydalis.io), using a few pictures
from my own collection; you can use this to see approximately what
Corydalis does and how does it look like.

## What it doesn't do

It doesn't actually modify your collection in any way. In other words,
it's not a "collection manager" (the way e.g. Adobe Lightroom is), but
rather is designed to work in conjunction with the actual manager.

## Installation and use

You will need
[stack](https://docs.haskellstack.org/en/stable/README/), a number of
common command line utilities, and to read the [installation
guide](docs/install.md). Then read the [manual](docs/manual.md) in
order to understand what Corydalis shows. You can also see the
documentation online at
[readthedocs](http://corydalis.readthedocs.io/).

## Bugs/features

There is a raw list of ideas/issues in the [todo
list](docs/todo.md). Otherwise, feel free to [file
bugs](https://github.com/iustin/corydalis/issues).

[![Build Status](https://travis-ci.org/iustin/corydalis.svg)](https://travis-ci.org/iustin/corydalis)
