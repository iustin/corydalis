# Composite images

A composite image is one picture made from several others: a
panorama, an HDR merge, a focus stack or an astro stack. Corydalis
recognises one by its name, which names the run of frames it was made
from, and links it to those frames, so that the frames don't look
like work still to do.

## Terms

- **Composite image**: a picture whose name is a range of other
  pictures' names, e.g. `dsc_0101-0105` for a panorama of `dsc_0101`
  to `dsc_0105`.
- **Component**: a picture whose name is in a composite image's
  range, in the same folder.
- **Master**: the file an image is made from: a file with one of the
  `rawexts` extensions (raw files, TIFF and DNG with the sample
  settings), or a JPEG in a source directory.
- **Output**: a processed file of an image, e.g. a JPEG in an output
  directory.
- **Statuses**: an image is *unprocessed* (a master and no output),
  *processed* (a master and an output), *standalone* (an output and no
  master) or *orphaned* (only a sidecar).

## Recognising a composite image

The name of the file, without its extension, is matched against the
`rangeregex` setting, which gives the root, the first number and the
last number; with the sample settings, `dsc_0101-0105` has the root
`dsc_`, and the numbers 0101 and 0105.

- The name is a range when the last number is not below the first,
  and has at least as many digits: `dsc_0001-2` is not a range, as
  camera names are padded, so the `-2` is a copy suffix. The names a
  range covers are padded to the width of the first number: `d_01-03`
  covers `d_01`, `d_02` and `d_03`, while `d_1-03` covers `d_1`, `d_2`
  and `d_3`.
- A camera counter that wrapped is a range too: when both numbers have
  exactly four digits, as camera file numbers do (0001 to 9999), and
  the last is below the first, the range runs up to 9999 and continues
  from 0001 (the last number can't be 0000). `dsc_9998-0002` covers
  `dsc_9998`, `dsc_9999`, `dsc_0001` and `dsc_0002`, in that order.
- A range has at most `maxrangesize` names (1024 by default), counting
  a wrapped range's names too; a longer one, e.g. a timestamp that
  looks like a range, is not a range. So `dsc_1920-0002` (8082 names)
  is not one.
- When the name is not a range, the copy suffix (the `copyregex`
  setting) is removed, and what is left is tried again. With the
  sample settings, `dsc_1920-2` is a copy of `dsc_1920` (2 is below
  1920), while `dsc_1920-1925` and `x_1-3` are ranges, and
  `d_01-03-Edit` is a copy of the composite image `d_01-03`. Note that
  a file meant as a copy of `x_1` and named `x_1-3` is a range; name
  it `x_1-Edit` instead, or change `copyregex`.

All of a composite image's files can be masters, outputs or sidecars,
so even a lone sidecar named like a range is a composite image, and
covers its components. Movies are not part of composite images: a
movie is never a composite image, even when its name is a range (or
another file of the same name, such as a poster, has one), and never
a component.

## The link

- The components of a composite image are the images of the same
  folder whose names are in its range. A name of the range with no
  image (e.g. `d_04` and `d_05` for `d_01-05` in a folder with only
  `d_01` to `d_03`) is ignored: the composite image covers the
  components that exist.
- It doesn't matter which source or output directories the composite
  image and its components are in, nor in which order they are read.
  Within the folder, though, they must be in the same sub-directory:
  `pano/d_01-03.jpg` covers `pano/d_01` to `pano/d_03`, not `d_01`
  to `d_03`.
- No file belongs to two images: the composite image has only its own
  files, and so does each component. The link is by name, in both
  directions.
- A component can be covered by several composite images (overlapping
  ranges), and is linked to all of them.
- Copies of a composite image's files belong to the composite image,
  as copies do for any image.
- A composite image is never another one's component.

The image page shows the link both ways: a composite image shows its
range (its first and last names, and how many names it has) and links
to its components, and a component links to the composite images
that cover it. Both lists show the first ten names, then how many
more there are.

## Statuses

- A component with an output of its own keeps its usual status:
  processed with a master, standalone without.
- A component with a master but no output of its own is processed as
  soon as a composite image covers it, even one without an output
  yet: the work that remains is the composite image's.
- A composite image with a master follows the usual rule: unprocessed
  without an output, processed with one.
- A composite image with an output but no master is processed when at
  least one of its components has a master (it was made from masters
  that are in the library), and standalone otherwise.
- Other images, such as one that is only a sidecar, keep their usual
  status.

Being a composite image or a component is not a status of its own.

## Counts and sizes

Every image counts once, in the class of its status; a composite image
is one image. Every file's size counts once, for the image that owns
it: a master in the raw size, an output in the processed or standalone
size of its image. These numbers, as all the values derived from a
folder's images, are computed once all of the folder's directories are
read, so that every page shows the same numbers.

## Dates and other metadata

- A composite image's metadata comes from its own files first. What
  they lack is taken from its first existing component (the first in
  the order of its range): the date, the camera, the lens and the
  place. The camera, the lens and the place are each taken whole, and
  only when the composite image's files have no field of it (e.g. no
  make, model or serial number for the camera). Nothing else is
  taken from a component: e.g. the size, orientation, exposure, focal
  length, rating, people and keywords.
- A component's metadata comes from its own files only: it doesn't
  get the people or keywords of the composite image, as the frames of
  a panorama don't each show what the panorama shows.
- A folder's year and first date are the earliest date among its
  images.

## Viewing

Every image is viewed from its own files: a component without an
output is viewed from its master, as any unprocessed image is.

## Examples

Each example is one folder whose source directory has three raws,
`d_01.nef` to `d_03.nef`, unless said otherwise. The sizes are in
made-up units: a raw is 10, a composite image's JPEG 50, a TIFF or DNG
100, a component's own JPEG 5, a movie 70.

| Files besides the three raws | Result |
|---|---|
| Output: `d_01-03.jpg` | 4 processed. Raw 30, processed 50. Each component is viewed from its raw |
| Source: `d_01-03.tif`. Output: `d_01-03.jpg` | 4 processed. Raw 130, processed 50 |
| Source: `d_01-03.tif`, and no output directory | 1 unprocessed (the TIFF), 3 processed. Raw 130 |
| Source: `d_01-03.tif`. Output: `d_01.jpg` only | 1 unprocessed (the TIFF), 3 processed. Raw 130, processed 5 |
| Output: `d_01-03.jpg` in one output directory, `d_02.jpg` in another | 4 processed. Raw 30, processed 55 |
| Output: `d_01-03.mov` | 3 unprocessed, 1 movie. Raw 30, movie 70 |
| Output: `d_01-03.jpg`, and no `d_01.nef` | 3 processed. Raw 20, processed 50 |
| Output: `d_01.jpg` to `d_03.jpg`, and `d_01-03.jpg` | 4 processed. Raw 30, processed 65. Each component is viewed from its own JPEG |
| Raws `x_1.nef` to `x_3.nef`. Output: `x_1-3.jpg` | 4 processed: `x_1-3` is a composite image of `x_1` to `x_3`, not a copy of `x_1` |
| Source: `d_01-03.jpg`, and no output directory | 1 unprocessed (the JPEG, a master here), 3 processed. Raw 80 |
| Output: `d_01-02.jpg` (50) and `d_02-03.jpg` (40) | 5 processed. Raw 30, processed 90. `d_02` is linked to both |
| Source: `d_01-03.dng`. Output: `d_01-03.jpg` | 4 processed. Raw 130, processed 50 |
| Output: `d_01-03.jpg` and its copy `d_01-03-Edit.jpg` (45) | 4 processed. Raw 30, processed 95 |
| No raws at all. Output: `d_01-03.jpg` | 1 standalone. Standalone 50 |

## Scope

This covers one kind of composite image: a run of frames numbered in
sequence, with one root, in one folder, combined into one picture.
That is an HDR merge, a panorama, a focus stack or an astro stack,
including one whose camera counter wrapped. Other kinds are not
covered:

- Frames in different folders, such as an astro stack shot over two
  nights: a link is a name inside one folder.
- A run with gaps, such as a panorama from `d_01`, `d_03` and `d_04`
  where `d_02` is a shot of something else: the range `d_01-04`
  covers `d_02` as well.
- Several roots, such as frames from two cameras, or from a phone and
  a camera.
- Nested composite images, such as a panorama made from three HDR
  merges: the panorama covers the frames, but not the HDR merges.
- Movies, as the result or as the frames.
- Results named another way by the camera or the editing tool, until
  they are renamed to the range form.
