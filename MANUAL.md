# Concepts

My work flow is that after processing, I export JPEG files for easy
viewing. Thus, the lack or presence of JPEG files is the criteria for
"has this picture been processed", and this concept is used for
categorising folder and so on.

## Image types

Depending on what kinds of files are present for an image, it will
categorised as follows:

* *raw* if we only have a raw file for it (sidecars are optional)
* *processed* if we have both raw and processed outputs (sidecars
  optional)
* *standalone* if we have a processed file but no raw file
* *outdated* if we have both a raw and processed file, but based on
  some heuristics (currently time stamp) the processed file is older
  than the most recent changes to the raw or sidecar file
* *orphaned* if we only have a sidecar file

A sidecar file is what image processing programs generate to store
metadata and/or history of changes; usually these are `xmp` files.

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
* a folder containing fully processed files, but some of them being
  outdated (per image types above) is classified as *outdated*

The classification of folders into only one of these categories is not
perfect; note for example that both *processed* and *outdated* folders
have fully processed files, the difference being in the freshness of
the JPEG copies. Similarly, an *unprocessed* folder can contain
standalone files, but is not classified as mixed in order to simplify
the work flow.

## File-system organisation

If both the raw and processed files are stored in the same file-system
directory, then it's very easy to manage the files using a file
manager, and Corydalis has less value. However, it can also manage
cases when the raw files and the jpeg outputs are tracked in
completely different trees, for example:

* raw files in /raw (/year/date)
* jpeg files in /jpeg (/year/date)

This helps with backups for example (and maybe permissions). Corydalis
will match a given directory across all trees it it watching based on
its name, which has to be consistent. Note that the depth of the tree
is important: Corydalis expects a three-level
`toplevel/intermediate-that-is-ignored/directory-that-is-tracked`
structure.

In a given directory (that it tracks), pictures can be organised
either flat (all in that directory) or across subdirectories. If
organised in subdirectories, the only difference is that the same
structure must exist in all instances of a directory, and the relative
path will be displayed in the interface.

# Work flow

After adding a new set of files to the file system, Corydalis will:

* if these are processed files already, mark the new folder as
  standalone; no further work needed
* if these are raw files, mark the new folder as raw; the top-level
  page will show the number of unprocessed files/folders

As a folder is being processed, it will move from raw to unprocessed
and then finally to processed. If all folders are in either processed
or standalone status, everything is perfect, no more work needs to be
done. However, this is not the case usually.

## Dealing with other states

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

### Outdated folders

If, after processing a folder, the raw files are revisited and some
are updated, then either the time stamp on the raw file or on the
sidecar file will be updated, depending on the raw file type and/or
the editing program. Corydalis will then flag the directory as outdated.

However, this doesn't work reliably:

* shooting directly in RAW+JPEG results sometimes in files with
  different time stamps (!), usually one second differences; there is
  an allowed delta configurable to account for this, however it's
  imperfect
* it's also nice to store the JPEGs in the file system with mtime set
  to the time that the picture was actually taken; this defeats the
  orphan mechanism completely.

Ideally, Corydalis would look at the EXIF time stamps instead of file
mtime, however this would make the image scanning much more expensive.

## Other processed files

There are two types of processed files that Corydalis treats
differently: additional copies and what it calls "range" files, used
mostly for panoramas or HDR combinations.

Additional copies are simple files that share the same base name but
usually with some suffix appended (configurable); these are not
treated then as standalone files but as more outputs from the same raw
file.

### Panorama/HDR outputs

The usual work flow for a panorama or HDR file is as follows:

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
