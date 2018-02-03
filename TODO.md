# Random notes and TODOs

## EXIF handling

- History When (XMP/Lightroom)
- Metadata Date (XMP/Lightroom)
- TODO: failure on zero-byte file
- To solve: how to promote file to image exif
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

### Multiple "RAW" files

This is related to copy handling. Let's say copy separator is '-', the
original file is 'img.nef', and a processed copy is 'img-edit.tiff'
was done. Depending on how the directory listing is, it can lead to
making 'img-edit.tiff' as main raw file, and 'img.nef' as processed
output (due to promotion raw→processed when we find such "duplicate"
raw files).
