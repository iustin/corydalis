# Random notes and TODOs

## Unit tests

The coverage over the entire codebase is quite low. Most of the
handlers are tested for very basic things, but the main logic of
images scanning/merging/etc. is not.

Some specific missing tests:

- the settings page: check that it shows the configuration;
- search: folders by year, and the quick search;
- flagged images: the JSON responses, once the test repository has
  images.

## EXIF handling

- History When (XMP/Lightroom)
- Metadata Date (XMP/Lightroom)
- Move exif to fully generic? Track all fields, re-introduce Object
  but binary serialization.
- ISO values given as two words (e.g. for high ISO modes) keep only
  the number (`parseIsoString`); record the mode too.
- The title, caption and other fields can differ between the files of
  an image (raw, sidecar, JPEG), but only one value is kept; how to
  show the differences in the viewer?
- Make exiftool write the metadata of each file separately, instead of
  parsing its combined output (`extractExifs`).

## Image viewer

- from lightroom: region type: face but without name, means unknown
  person.
- sub-image view (copies, raw file, etc.)
- skip panoramas unless explicitly requested
- vertical swipes are ignored; use them for something?
- keyboard panning moves by a fixed 25 (or 50, with shift) pixels; make
  it relative to the canvas size.
- image information (`exif` widget): show the serial numbers, link the
  camera and lens to their pages, and show the capture time earlier.

## Statistics

Camera/lens database:

- pics/week average and median

Stats:

- focal length
- camera+focal length
- camera/lens
- aperture
- shutter speed

Maybe generic (1 val, 2 val, 3 val, N val) auto-compute?

- with result field: picture count, etc.

## Search

- Event searches: images don't match event atoms (`imageSearchFunction`,
  which returns `False` for them), and there are no atom statistics for
  events (`getAtoms`); should event atoms find images
  (`atomFindsFiles`)?
- Year search: also match the year of the folder's event?
- Folder class search for images (`FClass`, which finds no images):
  based on the parent folder's class, or something else?

## Image cache

- Build the cached images concurrently (e.g. with
  `pooledMapConcurrentlyN_`).
- Make the search cache size (`emptySearchCache`, now 10 entries)
  configurable.
- Previews are also built in sizes larger than the image itself; skip
  those (this needs the image size in `Image`), and leave them out of
  the viewer's image sizes too (`Handler.Widgets`).
- Range files and their components get identical previews
  (`loadCachedOrBuild`).

## Curate page

- List the movies, as for the other kinds of files.

## Logging

- The application's own log messages (`makeLogFn`) carry no level, so
  under journald a warning and an info message look the same. Prefixing
  each line with the `sd-daemon` priority (`<4>` for warnings, `<6>`
  for info, etc.; see `sd-daemon(3)`) would let journald store the
  level, e.g. for `journalctl -p warning`. Only for journald (and maybe
  as an option), since other destinations would show the prefix as is.

## Folder cover image

- for consistency, should try to find a landscape-oriented image first
- and with a person, if possible

## Dependencies status

## Bugs

### Handling of copies

1. The current copy system is really bad. A copy is declared as such
   even when no master is present (for its deduced original name),
   leading to bad behaviour when multiple copies are possible;
   e.g. with '-' as separator, pic.xxx, pic-a.xxx, pic-a-b.xxx will
   lead to bad outcomes.

2. The copy regex handling is across the full "file path",
   i.e. including any subdirs under its parent folder. If the regex
   doesn't explicitly exclude slashes and if it also can catch dirs
   (e.g. '-' as separator, and subdirs that include dashes), it will
   lead to so broken behaviour that it is funny.
