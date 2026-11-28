# Composite images: links between images instead of shadow copies

Date: 2026-11-25 (specification agreed), implemented up to 2026-11-27.
Status: accepted, in PRs
[#104](https://github.com/iustin/corydalis/pull/104) (preparation),
[#106](https://github.com/iustin/corydalis/pull/106) (links) and
[#108](https://github.com/iustin/corydalis/pull/108) (browsing).
Written on 2026-11-28, from the specification, the commits and the
pull requests. The user-facing rules are in
[Composite images](../composite-images.md).

## Context

A range file is a picture whose name says it was made from a run of
frames: `d_01-03.jpg` is a panorama, HDR merge or stack of `d_01`,
`d_02` and `d_03`. The manual promised only a status rule: the frames'
raws are not "unprocessed", and the result is not "standalone".

The code implemented this by copying. Each range file created a
"shadow" per name of its range: a small image holding the range file
as its JPEG, merged later into the frame of that name. A result without
a master took its first frame's master. The values derived from a
folder's images (stats, time index, year, event) were computed before
these steps, per directory.

The rescan investigation (see
[The memory and cost of a rescan](2026-11-24-rescan-memory.md)) found
two bugs here (stale folder stats; shadows of one directory dropped
when merging folders) and planned patches for them. Instead, the
behaviour was first written down as a specification, with 14 cases
(one folder each) run through the code of the time by a scratch test:

- in 11 cases, the stored folder numbers and the numbers the folder
  page recomputed disagreed;
- in 11 cases, a size was counted more than once, or a frame was shown
  with another image's file (e.g. a range file of size 50 counted as
  200; all frames showing the panorama);
- a TIFF or a movie named like a range made frames "processed" and was
  viewed as if it were a JPEG; only the first frame could give a
  result its master; frames got the panorama's people and keywords;
  shadows stayed in memory and in the cache;
- with the sample regexes, `x_1-3` was a copy of `x_1`, not a range.

Most of these followed from the copies.

## Decision

A range is a link between images, and no file belongs to two images.
Terms: the result is a *composite image*, its frames are its
*components*; "range" is only the naming mechanism.

- **The link.** Once all directories of a folder are merged, one step
  (`finishFolder`) links the images by name: a composite image lists
  its existing components, each component the composite images
  covering it (`ImageRole`: `PlainImage`, `CompositeOf range
  components`, `ComponentOf composites`). Then it sets the statuses
  that depend on the link, fills the composite images' missing
  metadata, and computes the derived values once, from the final
  images.
- **Statuses.** The four statuses stay. A component with a master and
  no output counts as processed as soon as a composite image covers
  it, even one without an output yet; a composite image without a
  master is processed when any component has one.
- **Metadata.** A composite image takes the date, camera, lens and
  place of its first existing component when its own files lack them
  (each of the last three as a whole group), never the dimensions or
  orientation. Components get nothing from the composite image.
- **Names.** A range is tried first on the whole name, and accepted
  when the last number isn't below the first, or for a wrapped camera
  counter (both numbers exactly four digits: `dsc_9998-0002`);
  otherwise the copy suffix is removed and the range tried again. A
  padded name with a shorter last number (`dsc_0001-2`) is a copy. The
  size limit (1024 names) becomes the `maxrangesize` setting.
- **Movies** are never composite images or components, for now.
- **Browsing** (PR #108). A *hidden* component (covered, with no JPEG
  of its own) is left out of browsing, the viewer, search results,
  folder covers and the preview rendering after a scan; a search that
  finds one shows the composite images covering it. A search about the
  files themselves (by file name or problem, anywhere in the query)
  shows hidden components as they are. A component with its own JPEG
  stays visible.

## Alternatives

**Patching the shadows** (the fixes the rescan investigation had
planned: recompute the stats after the shadow steps, merge shadows in
folder merges, clear them afterwards). Rejected: it kept the copies, from which most of the
defects followed (double counted sizes, frames viewed from the range
file, the result taking one frame's dimensions).

**Making components count as processed only once the composite image
has an output.** Rejected: a composite image that exists (e.g. only
its TIFF) shows that the frames were used.

**A new visible status for covered components.** Rejected: the four
statuses are enough, with the link shown on the image pages.

**Only the first component giving a composite image its master.**
Rejected: which frame was kept on disk is an accident, not a different
kind of picture.

**Copy suffix first, then range** (the old order). Rejected: it made
`x_1-3` a copy. The cost of range first: a copy named like `x_1-3` in
an existing library becomes a range; padded camera names avoid the
clash, so such names are hand-made and rare.

**Giving components the composite image's people and keywords.**
Rejected: the tags describe the result, and a frame may not show what
the result shows. Searches find the composite image, whose page leads
to the components.

**Hiding components with their own output too.** Not done for now: an
output means its owner wanted it as a picture of its own. Both
readings have a case, so this may become a setting.

**Movies as composite images** (time-lapses, movies from clips). Left
out: the link is generic enough to add them later as a rule of their
own, but processed means something else for a movie.

**Out of scope:** components in other folders (for good: a link is a
name inside one folder), runs with gaps, several roots, nested
composite images, results named another way by the tool. All but the
first could later feed the same link from another source, such as a
declared list of components.

## Measurements

None beyond the 14 cases described in the context, run through the
code of the time (main before PR #104) by a scratch test that built
each folder from a list of files, merged its directories and applied
the shadow and range steps as a scan did.

## Consequences

- Folder numbers, the status page and the list page change for
  folders with range files (the changelog says so), and agree with the
  folder page.
- Components are viewed from their own files. The preview rendering
  after a scan skips hidden components; their previews are built from
  their raws when someone opens one.
- The repository cache's shape changed; old caches are rejected (see
  [A format version for the repository cache](2026-11-25-repository-cache-format-version.md)).
- A composite image counts once in the camera and lens statistics,
  with its own size, when it takes its camera from a component.
- Later ideas, not decided: show a composite image's exposure from its
  components; separate types for half-built and finished folders.
