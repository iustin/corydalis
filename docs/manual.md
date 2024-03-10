# Corydalis user manual

## Basic usage

If you are only interested in the image view capabilities, then
there's not much to it:

- open the web interface (after following the steps in the
  [installation guide](install.md))
- start browsing either from the year, person, location or keyword
  view
- or search by a combination of criteria, e.g. "Switzerland mountains
  2017", showing all pictures from (guessed) country Switzerland, with
  (guessed) keyword "mountains", taken in the (again guessed) year
  2017
- or, browse by folder type (in the "Curate library" section)

Once you select a search (or category), you're by default in image
browse mode (or, if the search you chose doesn't apply to images,
you're in folder browse mode. You can now scroll up/down now, and:

- in folder browse mode, clicking on a picture takes you to the
  dedicated image viewer (see below);
- in image browse mode, it opens up a "lightbox" with the image or
  movie (with integrated player) and you can switch between
  images (left/right, via keyboard or swipe) while the lightbox is
  open, in effect providing another infinite scroll experience.

This "browse mode" is one of the three available modes for looking at
things. The other two are:

- folder/image listing, which is an old-school tabular view, mostly
  for details on the files.
- and the dedicated image viewer, designed for fast, sequential view
  of images.

In the dedicated viewer, you see one image age the time. Available
controls:

- left/right (keyboard keys, or swipe for touchscreens): move backward
  (previous) and forward (next) in the list of pictures; this switches
  automatically to next folders, based on the current search;
- space key: move forward (next picture), or play movie (if the current
  picture is a movie);
- `f` (key), or tap the image with two fingers: go full-screen, if the
  browser allows it;
- `r` (key), or tab: go to a random image in the whole library;
- `u` (key): go back to the folder view;
- `p` (key): play the current movie (if any);
- `home`/`end` (key): go to the first, respectively last, image in the
  current folder;

And that is it. The image advancing flows continuously from one folder
to the next, so in theory you could go to the first every image in
your collection and keep scrolling through all of them.

Note: some browsers prevent alphanumeric key entry in full screen mode.
Thus, keys such as `u`, `p` or `r` don't work in this mode, and one has to
use "whitespace" keys such as left/right arrows, space, tab, etc.

If you want to understand more how Corydalis looks at pictures, read
on.

### Image metadata

Corydalis will use the image metadata (EXIF, IPTC, XMP, etc.) in order
to extract information and allow browsing along a limited number of
criteria: people present in the pictures (if they are tagged),
locations, keywords. I plan to expand this aspect further.

## Corydalis concepts

My workflow is that after processing the RAW, I export local JPEG
files for easy viewing. Thus, the lack or presence of JPEG files is
the criteria for "has this picture been processed", and this concept
is used for categorising folder and so on.

Also, take a look at the `config/settings.yml.sample` file, to
understand how some of the behaviours below can be configured.

### Search capability

Starting with version 0.3, the main navigation changed from the
folder-based to "search/filter" based. What this means is that once a
search has been made, the criteria are being propagated along
folder/image viewing, so you can easily view (or list) all images
tagged with keyword "flowers" and taken in a given year.

The search is image-based; i.e. all the search atoms operate on a
picture level, and if the filter is applied to a folder, if means
"does this folder contain any pictures that match the criteria?".

#### Full search API

The full search capability is not yet exposed in the UI, but is
available by manipulating the URL directly.

The search atoms are:

- country (string)
- province (string)
- city (string)
- location (string)
- person (set of strings)
- keyword (set of string)
- title (string)
- caption (string)
- year (numeric)
- season (set): the English words for the season
- month (set): the English words for the months
- day (set): the English words for the days of the week, or 'weekday'
  (equivalent to Mon-Fri), 'weekend' (Sat/Sun), or the numerical day
  of the month (e.g. 10, or even '10th' - this helps with targeting
  searches more precisely
- camera (string)
- lens (string)
- f-stop (numeric, e.g. `2.8`)
- shutter-speed (numeric), the shutter speed
- iso (numeric), the ISO value at which the picture was taken
- focal-length (numeric, in millimetres, e.g. `200`); this is either the native
  focal length, without conversion into 35-mm equivalent
- problem (this is what can be potentially errors; right now limited
  to exif metadata read issues)
- type (set: movie, image, or unknown)
- folder (string): matches folder name
- filename (string): matches image name (with any intermediate
  sub-paths)
- flash-source: internal, external, none, or unknown
- flash-mode (string): this is mostly descriptive

Numeric atoms (e.g. year, ISO, shutter speed, etc.) allow:

- equal (no prefix, or `=` prefix)
- not equal (prefixes allowed: `!=`, `/=` and `≠`)
- lower than (`<` prefix)
- lower or equal (`<=` and `≤`)
- greater or equal (`>=` and `≥`)
- greater than (`>` prefix)

Note that numeric not equal (`≠`) is different that `not` plus "equal to". Numeric
`≠` means a valid, defined value that is different, whereas `not` + "equal to"
will also match undefined (missing) values.

Fixed set atoms (e.g. season, month, type, etc.) only allow matches on one of
their values.

For set atoms (e.g. list of keywords for a picture), matching means one of their
elements matches the search.

String atoms (all others) allow:

- exact match (no prefix)
- fuzzy match (`~` prefix; does case-insensitive contains check, but
  not globs/regexes)

Both numeric and string atoms support the concept of a "missing"
value, but with some quirks. A missing atom usually means "this atom
is not available for this picture" (e.g. picture with no people in it,
or with no declared city), not all string atoms support this
meaningfully, for example the filename atom.

On top of that, arbitrarily complex combinations of atoms can be made
by operators such as:

- `and` (logical _and_ of two atoms)
- `or` (logical _or_ of two atoms)
- `all` (logical _and_ of multiple atoms)
- `any` (logical _or_ of multiple atoms)
- `not` (negates on atom)

As an example of a search: _((year 2017 and keyword beach) or (year 2018
and keyword winter)) and person xxx_.

The mapping to the URL is done as follows:

- an atom is declared by query parameter named as the atom, plus the
  requested value (eventually prefixed); e.g. `country=Italy` (exact
  match on country name), or `keyword=~mount` (fuzzy match, keyword
  contains "mount")
- missing (negative) atoms are the atom name prefixed with `no-`, with
  the value being ignored; e.g. `no-country`, meaning a picture not
  having an EXIF/IPTC country tag present
- the parsing of multiple atoms is done via reverse polish notation,
  i.e. multiple search atoms are pushed onto the stack, and a
  combining operator will pop as many atoms as it is designed to
- at the end, whatever is left on the stack is combined via an `all`
  atom

Example 1: _(country italy or country france) and year 2018_ is
represented by the query:
`country=italy&country=france&or&year=2018&and`.

Example 2: _keyword mountains and keyword not snow_:
`keyword=mountains&keyword=snow&not&and`.

Example 3: searching for all pictures taken on somebody's birthday,
e.g. _February 29th_: `month=february&day=29th&and`.

Note: due to the RPN parser, the order is critical in the parameters.

Note: the flash atoms are mostly Nikon-focused. Olympus for example is not
supported, as it doesn't use the "FlashSource" field, and instead fills the "did
use flash or not" using a "Flash" EXIF field that is free-form.

#### Atom searches on images versus folders

Folder and image search differ in semantics as what a complex filter
mean; they're mostly equivalent for simple filters, and a folder
search for X means it contains at least an image that passes X.

However, even for some simple cases this fails:

- an empty search filter, meaning "match all" (technically, _all_ with
  no parameters) will return also folders with no pictures; I consider
  this the natural behaviour when wanting to list all _folders_.
- a search for "no year information" will return the combination of
  the usual "folders containing pictures with no date information" and
  folders for which we can't determined the date at all (meaning no
  pictures with date information at all); again, I think this is more
  natural and allows cleaning mistakes in the repository.

These two exceptions show that the mapping of the atom types
between folders and images is not an exact `1:1`, so starting with
version 0.4 the meaning has changed significantly for complex
searches. Folder search now means returning the folders which can
satisfy the filter using a combination of their images, not via a
single image.

For example: _keyword=snow and year=2018_. For images, this means
finding an image taken in 2018 with keyword _snow_. For folders, it
means finding the folders with have both an image tagged with _snow_
and an image taken in 2018, possibly but not necessarily the same
image.

The downside of not being able to restrict the search so much is
mitigated by the fact that image searches, in both list and view mode,
act as "virtual folders". A proper fix would be to expand the search
language with more specific atoms, which is not a good solution
either.

#### Quick search

The quick search interface, available from the navigation bar,
provides a much simplified interface, with an accompanying reduction
in capability:

- the given string is split in words; for each word:
  - in case it contains a colon (':'), then it is split in two around
    it, and the prefix is tried as a keyword; if successful, this is
    the search element that will be used for the word
  - if not, each atom type is tried; if a single one matches, that is
    used, if multiple match, they're combined with `or`;
- the tries mentioned above are fuzzy matches for string atoms, and
  equality matches for numerical atoms
- the matches for words are combined with `and`, thus ensuring that
  each word will be required to match.

As an example, the search _switzerland 2018_ will be transformed into:

    (person switzerland or country switzerland or location switzerland
    or …) and (person 2018 or country 2018 or location 2018 or … or
    year 2018)

Assuming the usual case that _switzerland_ matches only country and a
keyword, and that 2018 is only a year, the resulting simplified filter
will actually be:

    (country switzerland or keyword switzerland) and year 2018

In case the input is given as `province:zürich 2018`, then this will
be directly tried as:

    (province zürich and (year 2018 or country 2018 or location 2018
    or …))

in effect skipping the discovery of which atoms would match for
"zürich" and directly using the province keyword.

Further tweaks to the search parameters can be done by modifying the
URL directly, per the previous section.

### Image status

The image "status" attribute is orthogonal to image viewing, and is
designed to help processing pictures. Just for image viewing, this
section can be entirely ignored. But the status atom reuses the same
status value, so this applies for searches as well.

Depending on what kinds of files are present for an image, it will
categorised as follows:

- _raw_ if we only have a raw file for it (sidecars are optional)
- _processed_ if we have both raw and processed outputs (sidecars
  optional)
- _standalone_ if we have a processed file but no raw file
- _orphaned_ if we only have a sidecar file

Note that movies are always considered processed; a proper fix would
be to track their status as well, but it doesn't seem needed to me
(given my workflow).

A sidecar file is what image processing programs generate to store
metadata and/or history of changes; usually these are `.xmp` files.

For most image types, Corydalis will be able to view them no matter
the state, either by using the JPEG file directly, or by extracting a
preview from the RAW file - most RAW file include a full HD, if not
full-size preview (almost the equivalent of what would happen if you
would have shot in RAW+JPEG, usually a slightly higher compression
ratio, but otherwise the same).

See the Workflow section for the case of JPEG-only images.

### Folders

Corydalis expects the repository to be organised along directories;
for each such directory that it recognises as a "picture folder", it
classifies it into a category based on the presence or not of all file
types:

- a folder that has only raw files, which is the normal state after a
  shooting session, is categorised as _raw_
- a folder that is in the process of having the pictures being
  processed, i.e. having JPEG files for part of the raw files, is
  _unprocessed_
- a folder with all raw files having processed output is _processed_

Beside these three obvious states, there are a few additional
categories:

- a folder having only processed files, the case when a session was
  shot directly in JPEG format, is classified as _standalone_
- a folder that has both raw files (with processed output) and
  processed files without a raw file is _mixed_
- a folder only containing no pictures is _empty_

The classification of folders into only one of these categories is not
perfect; for example, an _unprocessed_ folder can contain standalone
files, but is not classified as mixed in order to simplify the
work-flow.

## File-system organisation

If both the raw and processed files are stored in the same file-system
directory, then it's very easy to manage the files using a file
manager, and Corydalis has less value. It has more use when the raw
files and the jpeg outputs are tracked in completely different trees,
for example:

- raw files in `/raw` (`/year/date`)
- jpeg files in `/jpeg` (`/year/date`)

In this case, Corydalis will match a given directory across all trees
it it watching based on its name, which has to be consistent. Note
that the depth of the tree is important: Corydalis expects a
three-level
`toplevel/intermediate-that-is-ignored/directory-that-is-tracked`
structure.

In a given directory (that it tracks), pictures can be organised
either flat (all in that directory) or across subdirectories. If
organised in subdirectories, the only difference is that the same
structure must exist in all instances of a directory, and the relative
path will be displayed in the interface.

## Workflows

Note that no matter which of the workflows below you use, you will
need to "refresh" the internal state of Corydalis (via the `Reload
button in the top toolbar) once you add/remove/change files under the
directories it has configured. Currently it doesn't have do this
automatically.

Also, you can mix-and-match the workflows at any time, of course.

### RAW-only workflow

With a pure RAW workflow, things could look like this:

- You add some RAW files to your file system in a new directory;
  reloading Corydalis will show a new folder in state 'raw'.
- You start processing the files, and creating JPEG versions; the
  folder will become 'unprocessed' after the first JPEG file.
- You finish processing, and all files have both RAW and JPEG
  versions; the folder is not 'processed'.

At any stage, you can view the files, either based on the embedded
preview in the RAW file, or on the processed JPEGs (Corydalis
automatically determines what to use).

### RAW+JPEG workflow

You shoot both RAW files (for potential further processing) and JPEG
files, since most of the time you use OOC (out-of-camera) files.

- You add the RAW and JPEG files to the file system, either in the
  same location or separate locations.
- Corydalis will show the folder directly in 'processed' state (it
  doesn't help you much here); viewing the files will always use the
  JPEG files.

### JPEG-only workflow

Let's say you only shoot JPEG, or that you have some extra JPEG-only
files (from your phone) on top of the other pictures. How to handle
these?

For this particular case, Corydalis has the concept of a 'soft
master': a JPEG file that lives in one of the directories configured
as raw sources, with no RAW file, will also be considered a
source/virtual RAW for the purposes of the workflow. So, if there's no
copy in the JPEG directories, it will consider it 'unprocessed'.

## Other folder/image states

### For folders

#### Unprocessed folders with low count of unprocessed files

Sometime after processing files and generating JPEG outputs, while
reviewing the exported files I find some that I don't like (maybe
duplicates, maybe something that later I find not needed) I just
delete the JPEG file. This means the folder is back in 'unprocessed'
state, but if the unprocessed count is very small, it's most likely to
mean that JPEG file was intentionally deleted. Either re-generate the
JPEG file or delete the RAW file.

#### Mixed folders

The opposite scenario can happen as well: after exporting, some raw
files are deleted, but the JPEG file remains on the filesystem. This
puts the folder in the 'mixed' state. The only clean action here is to
delete the JPEG file, unless one has backups and restores the raw
file.

#### Empty folders

In some cases, deletion of pictures results in folders that are empty
but still present; Corydalis will flag this case to help delete these
folders.

However, it can be the case that only file types which are not
supported by Corydalis exist in these folders (e.g. movies); currently
Corydalis flags these folders incorrectly as 'empty', although it
shows the files in the folder view; ideally there would be a separate
'other' status for folders.

### For files

There are two types of processed files that Corydalis treats
differently: additional copies and what it calls "range" files, used
mostly for panoramas or HDR combinations.

Additional copies are simple files that share the same base name but
usually with some suffix appended (configurable); these are not
treated then as standalone files but as more outputs from the same raw
file.

#### Panorama/HDR outputs

My usual workflow for a panorama or HDR file is as follows:

- a number of RAW files are shot;
- these are processed and then combined into a panorama;
- depending on application, either the panorama itself is stored as
  "source format" (e.g. `TIFF` file), or only directly as processed
  output (`JPEG`);

Corydalis supports both these cases:

- for base file names `dsc_001` to `dsc_009`, an intermediate
  `dsc_001-009.tiff`, and output file `dsc_001-009.jpeg`, it will
  associate the `jpeg` file to the `tiff` one, and also to the
  individual raw files; this means that no raw/source file will be
  considered unprocessed.
- for base file names `dsc_001` to `dsc_009`, and output file
  `dsc_001-009.jpeg` (without intermediate `tiff` file), it will
  associate the `jpeg` file to all raw files; this means that the raw
  files will be considered processed, and that the `jpeg` file won't
  be considered as standalone.

## Old concepts

### Outdated outputs

Previously, Corydalis tries to track "outdated" files - that is, files
that based on some heuristics were deemed to need re-processing (the
algorithm was: the processed file is older than the most recent
changes to the raw or sidecar file, modulo a configurable delta).

However, this was too unreliable. It is perfectly valid to do some
work on the source file that would update the mtime of the
source/sidecar, but which however do not influence the output file, or
not enough to warrant reprocessing. Thus, tracking a "processed" vs an
"outdated" file doesn't make sense. On top of that, in some cases,
shooting directly in RAW+JPEG sometimes results in files with
different time stamps (!), usually a one second difference; there is
an allowed delta configurable to account for this, however it's yet
another work-around for an unreliable heuristic.
