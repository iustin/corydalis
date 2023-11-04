# Changelog

## v2023.44.0

*Sat, 04 Nov 2023*

A new release (at a rather arbitrary point in time), and switching to
calendar-based versioning since it makes more sense for this application.

### HiDPI displays

Finally fixed image resolution handling for hi-dpi displays. Previously,
Corydalis was completely oblivious to this, so the images were fuzzy in the
native (built-in) image viewer (issue #10). Now they should show correctly.

### Performance changes

Image thumbnails in image/folder list make use of the HTML-native
lazy-loading mechanism, which (for browsers that implement it) makes
large image lists actually usable. Not a proper replacement for a
paged loading mechanism, but a working substitute.

Slight performance change for loading pages due to reduced and
combined CSS/JS resource serving. This can lead to 10-20% page loaded
speed-ups, depending on the page type and complexity.

### Exif changes

Now parses the rating as written by (at least) Lightroom. Value '0' is
unrated, otherwise the explicit rating.

### Search changes

Added new search atoms _season_, _month_, _day_, _rating_,
_people-count_, _keyword-count_.

Season is usual *English* name for season, i.e. _winter_, _spring_,
_summer_, _autumn_, and the season computation is based on month
boundaries: winter lasts from December to January, and the rest
follow.

Month is usual *English* name for the month, or alternatively the
numeric value (1-12).

Day is a bit more complex. It can take any of: day-of-week (English
names), 'weekday', 'weekend', or numerical month day (e.g. '10th').

Rating is the exif rating as written by camera or image processing
tools, usually using the values 1-5, with 0 being unrated.

People and keyword count atoms are what you'd expect, searching by how
many people or keywords are in an image.

All the numeric search atoms (year, rating, counts, etc.) can use '<'
and '>' in the quick search, e.g. `keyword-count:>3`.

### UI interaction changes

Previously, searching for things, or clicking to various links was
always going to either an image grid browsing view or folder grid
browsing view, based on static conditions. Now, the preference of how
to display search results has moved to a per-browser sticky
(long-lived) cookies, so that multiple searches will remain on last
view. The preference is updated whenever the view is manually changed
(via the alternate links in the page).

In case of missing preferences, the default is same as before, image
or folder grid.

### Internal changes

The way image name URLs are built for multi-level paths changed;
instead of encoded '%2F' elements in the path, with the image being a
single path element, now image names are multi-path elements:
`a%2Fb%2Fc` becomes `a/b/c`. This should help with reverse-proxying
corydalis, and in general is a cleaner way to represent file-system
paths in URLS.

The source tree no longer contains embedded libraries (yay!). They
have been replaced by symlinks (or transformations) of upstream files
as shipped in [npm](https://www.npmjs.com/) packages. This should
simplify future development.

## v0.4.0

*Sun, 26 May 2019*

This is yet another major release, getting closer to a friendly UI for
photo viewing in parallel with the expansion of the library curation
for people so interested. Around 380 commits, so even bigger than
v0.3…

### New features

A large number of new features, due to the long bake time. Will do
better next time ☺

#### Movie support

It is now possible to view movies formats that are natively supported
by the browser. This is not much, as many older cameras were creating
things like `AVI` files, or some cameras still generate `MOV`s that
are not well supported.

Nevertheless, for the formats that are supported, it is possible to:

- view them as such, in the image viewer and the new browsing mode
- search for movies by type
- see statistics (in the 'curate' module)

When just looking at images (in the new browsing UI, or as thumbnails,
etc.) the movie "cover" is taken either from an embedded thumbnail (if
available), or from the first frame via `ffmpeg`, which is a new
dependency.

#### A new alternative UI for browsing pictures

The existing image viewer, designed for fast sequential viewing, lack
the ability to browse through images, without necessarily wanting to
see them in detail. Furthermore, the existing image/folder views with
their data-oriented tabular formats were really not user friendly, but
more geared towards analysing the photo library.

There's now a new, parallel "browse" mode that is grid based, which
allows scrolling through the pictures. It deals much better with large
results as it's paginated and incrementally loading, and applies the
same to folder views and to image views.

On the folder view, selecting an image leads into the image viewer for
that folder, whereas for the image view, it just opens the image or
the movie in a "lightbox", allowing to view the image in more detail
(even at full resolution, which is better than the previous image
view) without losing track of browsing position.

It is also possible to switch between folder and image views and
between the list-based and the browse-based view for either.

#### Asynchronous repository scanning with progress report

The previous synchronous-scanning done in the reload handler was, of
course, a big kludge, and it was finally time to retire it. The reload
handler now just triggers the scan, and it's possible to see
progress statistics for both the scanning phase, and for the rendering
phase, which was completely hidden before.

The refactoring had a couple of nice side-effects:

- the UI is fully usable during the scans; if this is a subsequent
  scan, the previous results are kept until the in-progress scan is
  finished, leading to no "down-time" of accessing the data;
- rendering phase is better controlled and there's no significant
  duplicate work; previous implementation was forgetting the rendering
  thread and could lead to N parallel threads all doing the same work;

On top of all this, the repository state is cached at end of scanning,
so initial load (after task restart) is _much_ faster.

#### New search atoms

More search atoms, which means higher chance of confusion. Anyway ☺

* added title and caption (string) atoms
* added folder (string) atom which allow usual searches on folder name
* added filename (string) atom which allows searching on image name,
  including any intermediate directories (such as
  `day1/dsc_0590.jpg`).
* added image status (set) atom, which replaces the old by-status view.
* added folder class (set) atom, which replaces the old by-folder-type
  view.
* added a lens (string) search atom.

The addition of image status and folder class atoms allowed completely
removing the old style "by-category" views, completing the unification
of the search/browse/list paradigm onto searching (via filters).

#### Expanded library statistics (curate module)

Added camera statistics, including tracking of shutter count if
available, and expanded the lens statistics. One can now look to see
which cameras generate best keepers, or to track equipment use/last
use, etc. Multiple cameras of the same model are handled now via
serial info, which however breaks as many cameras don't store their
serial in movie metadata, only in images.

The over-time usage for both cameras and lenses now show graphs, but
I'm not sure they're very readable, so it's more like raw v1.

Improved handling of exif errors or (new) plain inconsistencies
(e.g. location fields existing but empty). This should allow removing

There is so much more data that could be shown here, but not yet…

### Performance improvements

Some folder statistics are now pre-computed and cached, leading to
significant speedups (up to one order of magnitude) in some cases
(e.g. the folder list view).

Quick search atoms now can be prefixed with an atom keyword followed
by a colon to restrict searches to that specific atom, and not try all
atoms that match. E.g. 'year:2018 country:Italy'.

As described above, repository state is cached, so application restart
is much faster.

There have also been some overall performance improvements by changing
the used types (some `String`→`Text` migration, or list to sets,
etc.), but with no hard data.

### Bugs fixed

Folder cover display should now more consistently find an image to
display, even if the first image(s) are not viewable or if the image
filter doesn't actually select an image from the folder.

Master (as in 'soft master') images that are not proper raw files, but
instead something like png/tiff/etc. are now directly viewable. The
logic for this is hard coded for now, but finally one can see master
bitmaps.

Quick searches for two (but exactly two) words which neither finds
anything was triggering a but in the atom parser; this is was fixed by
removing arbitrary-length operators in the parser.

Images are now viewed in capture time order, not alphabetical
order. This could be called an improvement as well, but it was IMO a
bug in the first place.

Full screen mode in the view library is now done via the `screenfull`
javascript library, leading hopefully to expanded compatibility. On
top of that, a fake full screen mode is there for browsers which don't
support a native on - looking at you, Safari on iOS, why oh why?

Relaxed the parsing of some exif fields, hopefully allowing parsing of
metadata generated by more exotic writers.

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

#### Internal changes

The application state was moved from global variables (`MVar`s) to a
context structure, which unblocked testing of minimal repositories. As
such, there's a tiny bit of coverage improvement, but this serious
testing remains for the future.

Untracked files are now handled together with image files, which
should lead hopefully to a better handling of files, but has potential
for some bugs (e.g. one was found in list folder with regards to image
statistics).

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
