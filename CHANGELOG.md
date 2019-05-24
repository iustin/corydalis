# Changelog

## v0.4.0

*unreleased*

This is yet another major release, getting closer to a friendly UI
than a hackish one.

### New features

### Improvements

Quick search atoms now can be prefixed with an atom keyword followed
by a colon to restrict searches to that specific atom, and not try all
atoms that match. E.g. 'year:2018 country:Italy'.

### Bugs fixed

### Miscellaneous

#### Folder search semantics

Changed semantics of folder search. Before, a folder search (almost)
always meant "it contains at least one image that itself matches the
exact filter". Now, a folder search for a composite filter (e.g. "A
and B") means it contains an image matching "A" and also contains an
image matching "B".

The rationale for this change is that some atoms only have meaning at
folder level (e.g. folder class), and other have different meaning at
folder and image level (e.g. year), so the handling of atoms should be
as well different.

A (desired) side effect of the change is that many searches are faster
on folders now, since we can look at aggregate folder statistics in
many cases, rather than look at each individual image in turn.

#### 'Path' atom split into folder and filename

Previously, the Path atom was searching on both the folder and image
name. This had some drawbacks, most importantly its non-enumerability
but also the lack of being able to restrict the search to either of
its two components.

Thus, the atom was renamed to Folder (which matches only on the folder
name), and a new Filename atom FileName atom has been added that
matches on (full) image name. By full, it means with any potential
intermediate directories.

## v0.3.0

*Fri, 09 Mar 2018*

A major release, with close to 300 commits.

**Important**: due to internal changes, all the cached exif metadata
will be regenerated, so the first startup should be slower. This
should proceed automatically, but as there is no versioning of the
cache, it might be a good idea to remove the files manually (as
there's no automated cleanup procedure yet):

```
$ find /path/to/cache/dir -name '*-bexif' -delete
$ find /path/to/cache/dir -name '*-exif' -delete
```

New features:

* Rewritten search system; besides just location and people/keyword
  searches, a number of other atoms have been added, and now the
  search supports arbitrary combinations of these.
* Image viewing/browsing have moved from folder-based (view all images
  in folder A, move to folder B, etc.) to the above search/filter
  based: show me (and view in order) all images with keyword flowers,
  no matter in which folders they are located.
* Exif parsing has been reworked, and a number of new fields have been
  added (e.g. ISO, shutter speed; location fields are now split into
  country/province/city/etc.)
* Exif parsing failures are recorded, can be viewed on the curate page
  and searched by the 'problem' atom.
* The security requirements have been somewhat relaxed, allowing the
  application to run in non-secure (https) mode, recommended only when
  using behind a reverse proxy, and to allow non-logged in browsing,
  recommended for a website/demo site.

Improvements:

* Directory scanning has been parallelised, leading to 2.5× and more
  speed-ups.
* Exif metadata is now cached even in the case of failures, and is
  only regenerated in case the source file is updated; this should
  allow for (much) faster rescans if there are many images that fail
  parsing.
* This might have been present in 0.2 too, but: exif metadata is now
  updated automatically if the source file is updated.
* The CSS and part of JS dependencies are combined and served as
  single file; this (the CSS part) has eliminated some flickering when
  used with Firefox.
* Added a lens statistics page.
* Added an about page, pointing back to GitHub for the sources and
  explaining a bit the application.

Bugs fixed:

* Exif parsing had significant issues with related to people parsing
  (at image level), and with folder-level aggregation (which was
  actually plain broken); these should be fixed now.
* UI usability on small screens was very low, mostly fixed now (but
  this can be always improved).
* Image viewer handles keystrokes better, eliminating their interception
  when modifiers are used; e.g. `CTRL+r` for reload was previously
  also triggering a `r` random image view.

UI changes/additions:

* Moved to Bootstrap 4, which allowed fixing the UI responsiveness fixed.
* Moved to Font Awesome 5 (new icon look).
* Small updates to the other dependencies as well.


## v0.2.0

*Released Thu, 08 Feb 2018*

Initial public release, after a very long bake time ☺
