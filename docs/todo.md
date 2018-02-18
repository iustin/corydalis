# Random notes and TODOs

## Unit tests!

The code is, practically, not tested today. There's a modicum of a
test that checks the main web page gets a 200 OK, but nothing else.

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

## Searching

So many search types! not sure how to solve.

## Statistics

Camera/lens database:

- first introduction
- latest picture
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

A number of issues:

- doesn't show first valid image, but plain first one
- even better, for consistency, should try to find a landscape image
  first

## Dependencies status

### FontAwesome 5

Version 5 moves to svg+js, but this seems somewhat cumbersome to me:
either include a 600KiB JS file (non-min, non-compressed), or go
through the trouble of selecting and maintaining the list of used
icons.

In comparison, v4 is 66KiB/82KiB for the entire fonts (woff/woff2),
which allows using without any care of icon selection. Sigh.

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
