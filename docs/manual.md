# Basic usage

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

Once you open a folder, click on the thumbnail of any picture to start
viewing from it. The controls are:

- left/right (keyboard keys, or swipe for touchscreens): move backward
  (previous) and forward (next) in the list of pictures; this switches
  automatically to the next folder;
- space key: move forward (next picture);
- `f` (key), or tap the image with two fingers: go full-screen, if the
  browser allows it;
- `r` (key): go to a random image in the whole library;
- `u` (key): go back to the folder view;
- `p` (key): if the current image has a movie associated, open it in
  another browser window (which will be prevented by default in
  current browsers), in order to have a fully native view without the
  Corydalis controls;
- `home`/`end` (key): go to the first, respectively last, image in the
  current folder;

And that is it. The image advancing flows continuously from one folder
to the next, so in theory you could go to the first every image in
your collection and keep scrolling through all of them.

If you want to understand more how Corydalis looks at pictures, read
on.

## Image metadata

Corydalis will use the image metadata (EXIF, IPTC, XMP, etc.) in order
to extract information and allow browsing along a limited number of
criteria: people present in the pictures (if they are tagged),
locations, keywords. I plan to expand this aspect further.

# Corydalis concepts

My workflow is that after processing the RAW, I export local JPEG
files for easy viewing. Thus, the lack or presence of JPEG files is
the criteria for "has this picture been processed", and this concept
is used for categorising folder and so on.

Also, take a look at the `config/settings.yml.sample` file, to
understand how some of the behaviours below can be configured.

## Search capability

Starting with version 0.3, the main navigation changed from the
folder-based to "search/filter" based. What this means is that once a
search has been made, the criteria are being propagated along
folder/image viewing, so you can easily view (or list) all images
tagged with keyword "flowers" and taken in a given year.

The search is image-based; i.e. all the search atoms operate on a
picture level, and if the filter is applied to a folder, if means
"does this folder contain any pictures that match the criteria?".

### Full search API

The full search capability is not yet exposed in the UI, but is
available by manipulating the URL directly.

The search atoms are:

- country (string)
- province (string)
- city (string)
- location (string)
- person (set of strings)
- keyword (set of string)
- year (numeric)
- camera (string)
- problem (this is what can be potentially errors; right now limited
  to exif metadata read issues)
- type (set: movie, image, or unknown)
- path (matches on the image or folder name)

Numeric atoms (currently only year) allow:

- equal (no prefix)
- lower than (`<` prefix)
- greater than (`>` prefix)

Fixed set atoms (currently only type) only allow matches on one of
their values.

Set atoms implement matching by matching on any of their contents.

String atoms (all others) allow:

- exact match (no prefix)
- fuzzy match (`~` prefix; does case-insensitive contains check, but
  not globs/regexes)

The path atom is special; while it supports equality searches, this
doesn't make much sense since if you know the full path you might as
go select it directly from the image list. Also, no path doesn't make
sense; for it the fuzzy match is the only real useful search.

Both numeric and string atoms support the concept of a "missing"
value, see later below.

On top of that, arbitrarily complex combinations of atoms can be made
by operators such as:

- `and` (logical *and* of two atoms)
- `or` (logical *or* of two atoms)
- `all` (logical *and* of multiple atoms)
- `any` (logical *or* of multiple atoms)
- `not` (negates on atom)

As an example of a search: *((year 2017 and keyword beach) or (year 2018
and keyword winter)) and person xxx*.

The mapping to the URL is done as follows:

- an atom is declared by query parameter named as the atom, plus the
  requested value (eventually prefixed); e.g. *country=Italy* (exact
  match on country name), or *keyword=~mount* (fuzzy match, keyword
  contains "mount")
- missing (negative) atoms are the atom name prefixed with `no-`, with
  the value being ignored; e.g. *no-country*, meaning a picture not
  having an EXIF/IPTC country tag present
- the parsing of multiple atoms is done via reverse polish notation,
  i.e. multiple search atoms are pushed onto the stack, and a
  combining operator will pop as many atoms as it is designed to
- at the end, whatever is left on the stack is combined via an `all`
  atom

Example 1: *(country italy or country france) and year 2018* is
represented by the query:
`country=italy&country=france&or&year=2018&and`.

Example 2: *keyword mountains and keyword not snow*:
`keyword=mountains&keyword=snow&not&and`.

Note: due to the RPN parser, the order is critical in the parameters.

### Atom searches on images versus folders

Normally, a folder search will return all folders with at least one
image matching the search filter. However, there are two exceptions to
this case, in order to make the search concept more logical:

- an empty search filter, meaning "match all" (technically, *all* with
  no parameters) will return also folders with no pictures; I consider
  this the natural behaviour when wanting to list "all".
- a search for "no year information" will return the combination of
  the usual "folders containing pictures with no date information" and
  folders for which we can't determined the date at all (meaning no
  pictures with date information at all); again, I think this is more
  natural and allows cleaning mistakes in the repository.

These two exceptions show that the mapping of the atom types
between folders and images is not an exact `1:1`. It's possible that
more exceptions will be added in the future.

### Quick search

The quick search interface, available from the navigation bar,
provides a much simplified interface, with an accompanying reduction
in capability:

- the given string is split in words;
- each word is tried as all possible atom types; if a single one
  matches, that is used, if multiple match, they're combined with
  `or`;
- the tries mentioned above are fuzzy matches for string atoms, and
  equality matches for numerical atoms
- the matches for words are combined with `and`, thus ensuring that
  each word will be required to match.

As an example, the search *switzerland 2018* will be transformed into:

    (person switzerland or country switzerland or location switzerland
    or …) and (person 2018 or country 2018 or location 2018 or … or
    year 2018)

Assuming the usual case that *switzerland* matches only country and a
keyword, and that 2018 is only a year, the result will be:

    (country switzerland or keyword switzerland) and year 2018

Further tweaks to the search parameters can be done by modifying the
URL directly, per the previous section.

## Image status

The image "status" attribute is orthogonal to the search parameters,
and is designed to help processing pictures. Just for image viewing,
this section can be entirely ignored.

Depending on what kinds of files are present for an image, it will
categorised as follows:

* *raw* if we only have a raw file for it (sidecars are optional)
* *processed* if we have both raw and processed outputs (sidecars
  optional)
* *standalone* if we have a processed file but no raw file
* *orphaned* if we only have a sidecar file

A sidecar file is what image processing programs generate to store
metadata and/or history of changes; usually these are `.xmp` files.

For most image types, Corydalis will be able to view them no matter
the state, either by using the JPEG file directly, or by extracting a
preview from the RAW file - most RAW file include a full HD, if not
full-size preview (almost the equivalent of what would happen if you
would have shot in RAW+JPEG, usually a slightly higher compression
ratio, but otherwise the same).

See the Workflow section for the case of JPEG-only images.

## Folders

Corydalis expects the repository to be organised along directories;
for each such directory that it recognises as a "picture folder", it
classifies it into a category based on the presence or not of all file
types:

* a folder that has only raw files, which is the normal state after a
  shooting session, is categorised as *raw*
* a folder that is in the process of having the pictures being
  processed, i.e. having JPEG files for part of the raw files, is
  *unprocessed*
* a folder with all raw files having processed output is *processed*

Beside these three obvious states, there are a few additional
categories:

* a folder having only processed files, the case when a session was
  shot directly in JPEG format, is classified as *standalone*
* a folder that has both raw files (with processed output) and
  processed files without a raw file is *mixed*
* a folder only containing no pictures is *empty*

The classification of folders into only one of these categories is not
perfect; for example, an *unprocessed* folder can contain standalone
files, but is not classified as mixed in order to simplify the
work-flow.

# File-system organisation

If both the raw and processed files are stored in the same file-system
directory, then it's very easy to manage the files using a file
manager, and Corydalis has less value. It has more use when the raw
files and the jpeg outputs are tracked in completely different trees,
for example:

* raw files in `/raw` (`/year/date`)
* jpeg files in `/jpeg` (`/year/date`)

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

# Workflows

Note that no matter which of the workflows below you use, you will
need to "refresh" the internal state of Corydalis (via the `Reload
button in the top toolbar) once you add/remove/change files under the
directories it has configured. Currently it doesn't have do this
automatically.

Also, you can mix-and-match the workflows at any time, of course.

## RAW-only workflow

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

## RAW+JPEG workflow

You shoot both RAW files (for potential further processing) and JPEG
files, since most of the time you use OOC (out-of-camera) files.

- You add the RAW and JPEG files to the file system, either in the
  same location or separate locations.
- Corydalis will show the folder directly in 'processed' state (it
  doesn't help you much here); viewing the files will always use the
  JPEG files.

## JPEG-only workflow

Let's say you only shoot JPEG, or that you have some extra JPEG-only
files (from your phone) on top of the other pictures. How to handle
these?

For this particular case, Corydalis has the concept of a 'soft
master': a JPEG file that lives in one of the directories configured
as raw sources, with no RAW file, will also be considered a
source/virtual RAW for the purposes of the workflow. So, if there's no
copy in the JPEG directories, it will consider it 'unprocessed'.

# Other folder/image states

## Folders

### Unprocessed folders with low count of unprocessed files

Sometime after processing files and generating JPEG outputs, while
reviewing the exported files I find some that I don't like (maybe
duplicates, maybe something that later I find not needed) I just
delete the JPEG file. This means the folder is back in 'unprocessed'
state, but if the unprocessed count is very small, it's most likely to
mean that JPEG file was intentionally deleted. Either re-generate the
JPEG file or delete the RAW file.

### Mixed folders

The opposite scenario can happen as well: after exporting, some raw
files are deleted, but the JPEG file remains on the filesystem. This
puts the folder in the 'mixed' state. The only clean action here is to
delete the JPEG file, unless one has backups and restores the raw
file.

### Empty folders

In some cases, deletion of pictures results in folders that are empty
but still present; Corydalis will flag this case to help delete these
folders.

However, it can be the case that only file types which are not
supported by Corydalis exist in these folders (e.g. movies); currently
Corydalis flags these folders incorrectly as 'empty', although it
shows the files in the folder view; ideally there would be a separate
'other' status for folders.

## Files

There are two types of processed files that Corydalis treats
differently: additional copies and what it calls "range" files, used
mostly for panoramas or HDR combinations.

Additional copies are simple files that share the same base name but
usually with some suffix appended (configurable); these are not
treated then as standalone files but as more outputs from the same raw
file.

### Panorama/HDR outputs

My usual workflow for a panorama or HDR file is as follows:

* a number of RAW files are shot;
* these are processed and then combined into a panorama;
* depending on application, either the panorama itself is stored as
  "source format" (e.g. `TIFF` file), or only directly as processed
  output (`JPEG`);

Corydalis supports both these cases:

* for base file names `dsc_001` to `dsc_009`, an intermediate
  `dsc_001-009.tiff`, and output file `dsc_001-009.jpeg`, it will
  associate the `jpeg` file to the `tiff` one, and also to the
  individual raw files; this means that no raw/source file will be
  considered unprocessed.
* for base file names `dsc_001` to `dsc_009`, and output file
  `dsc_001-009.jpeg` (without intermediate `tiff` file), it will
  associate the `jpeg` file to all raw files; this means that the raw
  files will be considered processed, and that the `jpeg` file won't
  be considered as standalone.

# Old concepts

## Outdated outputs

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
