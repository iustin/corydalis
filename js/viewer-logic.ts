/* Copyright (C) 2013 Iustin Pop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

// The viewer's data types, state and computations (geometry, zooming,
// gestures), kept free of DOM access so that the unit tests can run them
// directly. The DOM side is in js/viewer-app.ts.

// The Haskell types.

export type Transform = [number, boolean, boolean];

export type AffineMatrix = [number, number, number, number];

type Url = string;

export type ImageInfo = {
  info: Url;
  bytes: Url;
  movie?: Url;
  view: Url;
  flag: Url;
  list: Url;
  browse: Url;
  name: string;
  transform: Transform;
  matrix: AffineMatrix;
  fullres: boolean;
  exifhtml: string;
};

export type ViewInfo = {
  year: string;
  yearurl: Url;
  folder: string;
  folderurl: Url;
  folderlist: Url;
  image: string;
  imageurl: Url;
  first: ImageInfo;
  prevfolder?: ImageInfo;
  prev?: ImageInfo;
  current: ImageInfo;
  next?: ImageInfo;
  nextfolder?: ImageInfo;
  last: ImageInfo;
};

/** A logging function, as console.log. */
export type Logger = (...args: unknown[]) => void;

// Constants for gesture detection
/** Maximum duration (ms) for a gesture to be considered a swipe rather than a pan */
export const TAP_DURATION_THRESHOLD = 300;
/** Maximum time (ms) between taps to be considered a double-tap */
export const DOUBLE_TAP_THRESHOLD = 300;
/** Maximum movement (px) for a gesture to be considered a tap rather than a swipe */
export const TAP_MOVEMENT_THRESHOLD = 100;
/** Minimum change in distance between pointers to trigger zoom */
export const PINCH_ZOOM_THRESHOLD = 10;

/** The zoom change of one zoom in/out step. */
export const ZOOM_FACTOR = 1.1;
/** The maximum zoom (relative to fitting the image in the canvas). */
export const MAX_ZOOM = 100;

/** Represents a 2D dimension, i.e. a `{x, y}` pair */
export class Dimensions {
  /** The horizontal (width) dimension */
  x: number;
  /** The vertical (height) dimension */
  y: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  /** Creates a deep copy of this Dimensions object */
  public clone(): Dimensions {
    return new Dimensions(this.x, this.y);
  }

  /** Returns the longest dimension */
  public longest(): number {
    return this.x > this.y ? this.x : this.y;
  }

  /** Return the absolute magnitude */
  public absMagnitude(): number {
    return Math.abs(this.x) + Math.abs(this.y);
  }

  /** Returns a copy with the dimensions swapped */
  public swapped(): Dimensions {
    return new Dimensions(this.y, this.x);
  }

  /** Returns a copy with dimensions scaled by a given number */
  public scaled(scale: number): Dimensions {
    return new Dimensions(
      Math.round(this.x * scale),
      Math.round(this.y * scale),
    );
  }

  /** Returns a copy with negated values */
  public negated(): Dimensions {
    return new Dimensions(-this.x, -this.y);
  }

  /** Check for equality between Dimensions */
  public equals(other: Dimensions): boolean {
    return this.x === other.x && this.y === other.y;
  }

  /** Returns a copy with dimensions scaled each by another dimension */
  public dividedBy(div: Dimensions): Dimensions {
    return new Dimensions(this.x / div.x, this.y / div.y);
  }

  /** Returns a copy with dimensions decreased each by another dimension */
  public minus(sub: Dimensions): Dimensions {
    return new Dimensions(this.x - sub.x, this.y - sub.y);
  }

  /** Returns a copy with dimensions increased each by another dimension */
  public plus(add: Dimensions): Dimensions {
    return new Dimensions(this.x + add.x, this.y + add.y);
  }

  /** Return a copy with dimensions clamped on the bottom by the given value */
  public clampMin(min: number): Dimensions;
  public clampMin(min: Dimensions): Dimensions;
  public clampMin(min: number | Dimensions): Dimensions {
    if (typeof min === 'number') {
      return new Dimensions(
        this.x < min ? min : this.x,
        this.y < min ? min : this.y,
      );
    } else {
      return new Dimensions(
        this.x < min.x ? min.x : this.x,
        this.y < min.y ? min.y : this.y,
      );
    }
  }

  /** Return a copy with dimensions clamped on the top by the given value */
  public clampMax(max: number): Dimensions;
  public clampMax(max: Dimensions): Dimensions;
  public clampMax(max: number | Dimensions): Dimensions {
    if (typeof max === 'number') {
      return new Dimensions(
        this.x > max ? max : this.x,
        this.y > max ? max : this.y,
      );
    } else {
      return new Dimensions(
        this.x > max.x ? max.x : this.x,
        this.y > max.y ? max.y : this.y,
      );
    }
  }

  /** Return a copy with dimensions clamped between the given Dimension */
  public clampLoHi(boundary: Dimensions): Dimensions {
    return this.clampMin(boundary.negated()).clampMax(boundary);
  }
}

// Helper function to calculate distance between two points
export function getDistanceBetweenPoints(
  p1: Dimensions,
  p2: Dimensions,
): number {
  return Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2));
}

// Helper function to compute the midpoint between two points
export function getMidpoint(p1: Dimensions, p2: Dimensions): Dimensions {
  return new Dimensions((p1.x + p2.x) / 2, (p1.y + p2.y) / 2);
}

// Helper function to limit a number between two values
export function limitNumber(min: number, max: number, num: number): number {
  return Math.min(Math.max(num, min), max);
}

// Internal state

export type State = {
  fullscreen: boolean;
  img: HTMLImageElement;
  video: HTMLVideoElement | null;
  lastX: number;
  msgTimeId: number;
  transform: Transform;
  matrix: AffineMatrix;
  url: string;
  /** Cache for {@link window.devicePixelRatio} */
  devicePixelRatio: number;
  /** Current, or next scale to apply to the image. 1 means fit to canvas,
   * not 1:1 pixels. */
  scale: number;
  /** Last drawn scale. Used for correctly moving the {@link panOffsets}
   * during zooming. */
  lastScale: number;
  /** The internal bitmap dimesions, scaled from the the CSS pixels on
   * high-DPI displays */
  canvasSize: Dimensions;
  /** The image native dimesions, before any downscaling to fit in the
   * canvas */
  imageSize: Dimensions;
  /** Holds the amount of panning available based on image-vs-canvas
   * sizes. This represents half of the total limit, i.e. it's a ± of this
   * value.
   */
  panLimits: Dimensions;
  /** Holds the current pan offsets, in absolute pixels. A positive value
   * means the image is shifted to right/bottom, i.e. more of the left/top
   * side is visible. The absolute value of the components must be within
   * the {@link State.panLimits} value. */
  panOffsets: Dimensions;
  /** The scale for a 1:1 pixel mapping */
  scale11: number;
  /** Tracks the last pointer location. This helps on desktop browsers to
   * zoom around the mouse cursor. */
  pointerLocation: Dimensions;
};

/** What the viewer's state needs from the page when it's created. */
export type CoryEnv = {
  /** The page's URL. */
  url: string;
  /** The window's devicePixelRatio. */
  devicePixelRatio: number;
  /** Creates a new image element (as `new Image()`). */
  newImage: () => HTMLImageElement;
};

export class Cory {
  info: ViewInfo;
  prev: HTMLImageElement;
  next: HTMLImageElement;
  state: State;

  constructor(bootinfo: ViewInfo, env: CoryEnv) {
    this.info = bootinfo;
    this.prev = env.newImage();
    this.next = env.newImage();
    this.state = {
      fullscreen: false,
      img: env.newImage(),
      video: null,
      lastX: 0,
      msgTimeId: 0,
      transform: bootinfo.current.transform,
      matrix: bootinfo.current.matrix,
      url: env.url,
      devicePixelRatio: env.devicePixelRatio,
      scale: 1.0,
      lastScale: 1.0,
      canvasSize: new Dimensions(0, 0),
      imageSize: new Dimensions(0, 0),
      panLimits: new Dimensions(0, 0),
      panOffsets: new Dimensions(0, 0),
      scale11: 1.0,
      pointerLocation: new Dimensions(0, 0),
    };
  }

  /** This converts the given canvas xy coordinates back into screen
   * (CSS) coordinates. Slow, so should not be called in a hot path. */
  public convertToCss(location: Dimensions): Dimensions {
    const location00 = location.plus(this.state.canvasSize.scaled(1 / 2));
    return location00.scaled(1 / this.state.devicePixelRatio);
  }

  /**
   * modifyOriginX - changes the originX coordinate, and returns whether
   * a change was applied or not.
   */
  public panX(x: number): boolean {
    const oldX = this.state.panOffsets.x;
    const lim = this.state.panLimits.x;
    this.state.panOffsets.x = limitNumber(
      -lim,
      lim,
      oldX + x * this.state.devicePixelRatio,
    );
    return oldX !== this.state.panOffsets.x;
  }
  /**
   * modifyOriginY - changes the originY coordinate, and returns whether
   * a change was applied or not.
   */
  public panY(y: number): boolean {
    const oldY = this.state.panOffsets.y;
    const lim = this.state.panLimits.y;
    this.state.panOffsets.y = limitNumber(
      -lim,
      lim,
      oldY + y * this.state.devicePixelRatio,
    );
    return oldY != this.state.panOffsets.y;
  }
  /**
   * modifyOrigin - changes the originX/Y coordinates, and returns whether
   * a change was applied or not.
   */
  public pan(x: number, y: number): boolean {
    const didPanX = this.panX(x);
    const didPanY = this.panY(y);
    return didPanX || didPanY;
  }
}

/** The parts of the state that {@link layoutImage} reads and updates. */
export type LayoutState = Pick<
  State,
  | 'canvasSize'
  | 'imageSize'
  | 'scale11'
  | 'scale'
  | 'lastScale'
  | 'panLimits'
  | 'panOffsets'
  | 'devicePixelRatio'
>;

/** Where and how to draw an image on the canvas, see {@link layoutImage}. */
export type ImageLayout = {
  /** The translation of the canvas transform: the canvas centre. */
  origin: Dimensions;
  /** The draw offsets, relative to the origin. */
  offsets: Dimensions;
  /** The size of the drawn image (before any rotation). */
  size: Dimensions;
};

/**
 * Computes the placement of an image on the canvas, for the state's
 * canvas size, zoom and pan. Records in the state the image's native
 * size, the 1:1 scale, and the new pan limits, offsets (kept within the
 * limits) and last drawn scale.
 *
 * @param imgSize the image size, before any rotation
 * @param rotation the image rotation (0 if not rotated)
 * @param centreAt if given, the point (in CSS pixels) to zoom around
 */
export function layoutImage(
  state: LayoutState,
  imgSize: Dimensions,
  rotation: number,
  centreAt: Dimensions | null,
  log: Logger = () => {},
): ImageLayout {
  // Here begin the computations for the image placement.
  /// This is the real canvas size, in bitmap pixels.
  const contextSize = state.canvasSize;
  /** Image size, taking rotation into account */
  const imgRotated = rotation == 0 ? imgSize : imgSize.swapped();
  // And record the image native size.
  state.imageSize = imgRotated.clone();
  // Compute the absolute (unzoomed) 1:1 scale.
  state.scale11 = imgRotated.dividedBy(contextSize).longest();
  /** The image scale, taking into account any zooming */
  const scale = state.scale11 / state.scale;
  // Note: target* must be in original coordinate system, not
  // rotated! So using img.width, not imgW. This is because from
  // the point of view of the image, it's drawn straight, not
  // rotated. Sigh, head hurts.
  /** The image target size, after scaling */
  const targetSize = imgSize.scaled(1 / scale);

  log(
    'Pre-draw, contextSize: %o imgSize(R=%i): %o imgscaling: %f zoom: %f oldzoom: %f targetSize: %o',
    contextSize,
    rotation,
    imgRotated,
    scale,
    state.scale,
    state.lastScale,
    targetSize,
  );
  // The halved context size in transform then doing the opposite in
  // drawOffsets is required for rotated images. For straight images,
  // this could be skipped, for rotated, it's needed to center the image
  // rotation point, so that rotation is around the center of the
  // screen, not the top-left corner. Otherwise, rotation (e.g. CCW 90)
  // would move the image "on top" of the actual screen.
  const halvedContext = contextSize.scaled(0.5);
  /** The centered draw offsets (numerical) for the image, whether
   * smaller or bigger than the canvas, in a positive axis system. If
   * the image is smaller than the canvas on a given axis, the value
   * will be positive, and viceversa. For an image fully fitting the
   * canvas, the value will be zero on that axis. */
  const centeringPosition = targetSize.negated().scaled(1 / 2);
  /** The overflows of the image over the canvas, if any. Underflows
   * (whitespace) are zeroed. */
  const overflows = targetSize.minus(contextSize).clampMin(0);
  const oldLimits = state.panLimits;
  state.panLimits = overflows.scaled(1 / 2);

  if (centreAt !== null) {
    // We need to centre the zooming around the old coordinates at
    // centreAt, which are in display positive coordinates.
    const canvasCentreAt = centreAt
      .scaled(state.devicePixelRatio)
      .minus(halvedContext);
    // We need to take into account the current panOffsets.
    const canvasAndPan = canvasCentreAt.minus(state.panOffsets);
    // Now attempt to scale the panOffsets to keep the same position
    // _after zooming_. The way to do this is to move "out" the offsets
    // by newScale/oldScale.
    const newOffsets = canvasAndPan
      .scaled(state.scale / state.lastScale - 1)
      .negated();
    // And finally, add back the old pan offsets, since above we only
    // compute the delta, not the absolute move.
    const newWithOldPan = newOffsets.plus(state.panOffsets);
    log(
      'Zooming transformations: input %o, scale to display pixel %o, ' +
        'shifted left %o, with pan %o, scales %f→%f, shifted and scaled %o',
      centreAt,
      centreAt.scaled(state.devicePixelRatio),
      canvasCentreAt,
      canvasAndPan,
      state.lastScale,
      state.scale,
      newWithOldPan.negated(),
    );
    state.panOffsets = newWithOldPan;
  } else {
    // Re-check and limit panning to stay within panLimits, but try to
    // scale them based on the limits change, as that keeps a somewhat
    // constant offset zoom position.
    // TODO: think about rewriting this as we have lastScale now, which
    // is a more direct way to compute this.
    state.panOffsets.x *=
      oldLimits.x !== 0 ? state.panLimits.x / oldLimits.x : 1;
    state.panOffsets.y *=
      oldLimits.y !== 0 ? state.panLimits.y / oldLimits.y : 1;
  }
  state.panOffsets = state.panOffsets.clampLoHi(state.panLimits);
  state.lastScale = state.scale;

  const finalDrawOffsets = centeringPosition.plus(state.panOffsets);
  log(
    'Panning offsets: overflows=%o, centered=%o, pan=%o, offsetDraw=%o',
    overflows,
    centeringPosition,
    state.panOffsets,
    finalDrawOffsets,
  );
  return { origin: halvedContext, offsets: finalDrawOffsets, size: targetSize };
}

/**
 * Clamps a requested zoom to the allowed range, [1, {@link MAX_ZOOM}]
 * (0, the initial value, also becomes 1). Returns the clamped zoom, and
 * the message to show the user, if any: when trying to zoom out further
 * than the current zoom of 1, or beyond the maximum zoom.
 */
export function clampZoom(
  requested: number,
  current: number,
): { scale: number; message: string | null } {
  let scale = requested;
  let message: string | null = null;
  // Check if at initial, or weird state.
  if (scale == 0) {
    scale = 1;
  }
  if (scale < 1) {
    scale = 1;
    // Only show the message if we already were at minimum scale;
    if (current == 1) {
      message = 'Already at minimum zoom';
    }
  } else if (scale > MAX_ZOOM) {
    message = 'Maximum zoom reached';
    scale = MAX_ZOOM;
  }
  return { scale, message };
}

/**
 * Returns the URL of an image, asking for a resolution fitting a canvas
 * of the given size (in bitmap pixels).
 */
export function imageUrlScaled(
  baseUrl: string,
  canvasSize: Dimensions,
): string {
  const maxSide = canvasSize.longest();
  const url = new URL(baseUrl);
  url.searchParams.set('res', maxSide.toString());
  return url.toString();
}

/** The persistent message shown for movies. */
export const MOVIE_MESSAGE =
  "This is a movie. Press 'p', click or touch to play.";

/** Returns the persistent message for an image: whether it's a movie. */
export function movieMessage(info: Pick<ImageInfo, 'movie'>): string {
  return info.movie != null ? MOVIE_MESSAGE : '';
}

/**
 * Returns the zoom ratio for a mouse wheel (or trackpad) scroll, or null
 * if the scroll is not (mostly) vertical, and so should not zoom.
 */
export function wheelZoomRatio(deltaX: number, deltaY: number): number | null {
  const absX = Math.abs(deltaX);
  const absY = Math.abs(deltaY);
  // Handle vertical scroll, but only if it's the dominant scroll
  // type, so that left-right scroll on the trackpad doesn't zoom by
  // minute amounts.
  if (deltaY != 0 && absY > 2 * absX) {
    // The various devices generate different values: one mouse wheel
    // "click" is about "4" units, but a fast one is 300, whereas a
    // slow trackpad scrolls is is 1. So try to balance them all.
    const capped = Math.min(absY, 10);
    const ratio = 1 + capped / 100;
    return deltaY > 0 ? ratio : 1 / ratio;
  }
  return null;
}

/**
 * The action for the end of a single pointer gesture, see
 * {@link classifyPointerUp}.
 */
export type PointerUpAction =
  'next' | 'previous' | 'vertical-swipe' | 'tap' | 'double-tap' | null;

/**
 * Classifies the end of a single pointer gesture:
 *
 * - a short gesture with a large movement is a swipe; a horizontal swipe
 *   moves to the next (leftwards) or previous (rightwards) image, but
 *   only when not zoomed in, as then the gesture pans the image instead;
 * - a short gesture with a small movement is a tap, or a double tap if
 *   shortly after a previous tap;
 * - anything else (a long gesture) needs no action, as it's a pan, which
 *   is handled while the pointer moves.
 *
 * @param duration how long the gesture took (ms)
 * @param delta the pointer movement
 * @param sinceLastTap the time since the last tap (ms)
 * @param scale the current zoom
 */
export function classifyPointerUp(
  duration: number,
  delta: Dimensions,
  sinceLastTap: number,
  scale: number,
): PointerUpAction {
  // Pan end: handled during the pointermove events.
  if (duration >= TAP_DURATION_THRESHOLD) {
    return null;
  }
  const totalMovement = Math.sqrt(delta.x * delta.x + delta.y * delta.y);
  if (totalMovement > TAP_MOVEMENT_THRESHOLD) {
    // But make sure we're not in zoomed mode, in which case the
    // action is pan, and was handled in the 'pointermove' event
    // handler.
    if (scale !== 1.0) {
      return null;
    }
    if (Math.abs(delta.x) > Math.abs(delta.y)) {
      return delta.x > 0 ? 'previous' : 'next';
    }
    // This is a no movement, or vertical movement swipe.
    return 'vertical-swipe';
  }
  return sinceLastTap < DOUBLE_TAP_THRESHOLD ? 'double-tap' : 'tap';
}

/** Returns the position of a movie (as a percentage), for the seek bar. */
export function seekPercent(currentTime: number, duration: number): number {
  return (100 / duration) * currentTime;
}

/** Returns the movie time for a seek bar position (a percentage). */
export function seekTime(duration: number, percent: number): number {
  return duration * (percent / 100);
}
