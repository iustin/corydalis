# Introduction

Corydalis is a simple application to help manage offline (as in
non-cloud) collections of RAW and processed pictures. It doesn't
replace your RAW processor (e.g. Adobe Lightroom, DxO PhotoLab,
Capture one, Darktable, etc.) but instead acts as a companion to it,
giving a parallel view on all your files.

Corydalis is also an image viewer, designed to work across and
understand your entire photo collection; the desktop-only viewers
(e.g. Geeqie) are folder-based, not collection-based.

Originally, I wrote it as I'm not able to keep post-processing up with
the amount of RAW pictures I take; additionally there is a lot of
cruft accumulating over time. It evolved to be an image viewer as I
needed one that both understands the entire collection, and can export
(as a web application) the pictures stored on your storage to desktop
and mobile clients - I want the ease of viewing/browsing pictures in a
browser, without having to upload my photos to the cloud.

Corydalis should be mostly independent of the actual file types and
the program used to process the raw files, although maybe I do make
some assumptions and I'm just not aware of it; its only requirement is
that _ImageMagick_ (used to generate the downscaled images for web
viewing) supports the "processed" image format (which in the vast
majority of cases will be JPEG, so the point is moot), and that
_exiftool_ can extract metadata from your images.

See [the installation guide](install.md) file for how to quickly
run and see Corydalis' output; the application only writes to its own
`static/tmp` directory and the 'cachedir' directory as specified in
the configuration files. Pointing that key to an initially empty
directory ensures that it will not clobber anything.

The application itself is a web server - the allowed users are stored
in an sqlite database in the `db/` directory - and is designed to be
run close or on the machine that hosts the files (walking the file
system over NFS or Samba will a long time); it can be run directly on
the NAS if it's a Linux machine.

Once started, navigate to the host/port setup in the configuration
file, and explore the classification of folders. Note that not all
organisation of files/folders is supported, see the
[manual](manual.md).

## Demo site

There is a demo site at
[demo.corydalis.io](https://demo.corydalis.io), using a few pictures
from my own collection; you can use this to see approximately what
Corydalis does and how does it look like.

## Features

* file-type independent (customisable extensions for raw files,
  processed files, sidecar files, etc.), as long as ImageMagick and
  exiftool can work with the files (parse EXIF data, process the
  files, etc.).
* independent of the program that edits the files.
* tracks files also for panorama/HDR outputs (multiple raw files
  combined into a single output file).
* computes list of unprocessed files, orphan sidecar files, etc.
* view the output/processed files, with navigation facilities allowing
  the browsing of the entire collection (requires a recent browser
  with HTML5 support; tested on Firefox, Chrome and partially Safari).
