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
