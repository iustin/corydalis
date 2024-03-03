# Random notes and TODOs

## Unit tests

The coverage over the entire codebase is quite low. Most of the
handlers are tested for very basic things, but the main logic of
images scanning/merging/etc. is not.

## EXIF handling

- History When (XMP/Lightroom)
- Metadata Date (XMP/Lightroom)
- Move exif to fully generic? Track all fields, re-introduce Object
  but binary serialization.

## Image viewer

- from lightroom: region type: face but without name, means unknown
  person.
- sub-image view (copies, raw file, etc.)
- skip panoramas unless explicitly requested

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
