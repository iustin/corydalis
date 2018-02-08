# Basic usage

If you are only interested in the image view capabilities, then
there's not much to it:

- open the web interface (after following the steps in the
  [installation guide](install.md))
- start browsing either from the year, person, location or keyword
  view
- or, browse by folder type (in the "Curate library" section)

Once you open a folder, click on the thumbnail of any picture to start
browsing from it. The controls are:

- left/right (keyboard keys, or swipe for touchscreens): move backward
  (previous) and forward (next) in the list of pictures; this switches
  automatically to the next folder;
- space key: move forward (next picture);
- `f` (key), or tap the image: go full-screen, if the browser allows
  it;
- `r` (key): go to a random image in the whole library;
- `u` (key): go back to the folder view;
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

## Image types

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
