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

/// <reference types="jquery"/>
/// <reference types="screenfull"/>
/// <reference types="bootstrap"/>

// The Haskell types.

type Transform = [number, boolean, boolean];

type AffineMatrix = [number, number, number, number];

type Url = string;

type ImageInfo = {
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
};

type ViewInfo = {
  year: string;
  yearurl: Url;
  folder: string;
  folderurl: Url;
  folderlist: Url;
  folderbrowse: Url;
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

// Internal state

type State = {
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
  scale: number;
  /** The internal bitmap dimesions, scaled from the the CSS pixels on high-DPI displays */
  canvasSize: Dimensions;
  /** The image native dimesions, before any downscaling to fit in the canvas */
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
};

class Cory {
  info: ViewInfo;
  prev: HTMLImageElement;
  next: HTMLImageElement;
  state: State;

  constructor(bootinfo: ViewInfo) {
    this.info = bootinfo;
    this.prev = new Image();
    this.next = new Image();
    this.state = {
      fullscreen: false,
      img: new Image(),
      video: null,
      lastX: 0,
      msgTimeId: 0,
      transform: bootinfo.current.transform,
      matrix: bootinfo.current.matrix,
      url: location.href,
      devicePixelRatio: window.devicePixelRatio,
      scale: 1.0,
      canvasSize: new Dimensions(0, 0),
      imageSize: new Dimensions(0, 0),
      panLimits: new Dimensions(0, 0),
      panOffsets: new Dimensions(0, 0),
      scale11: 1.0,
    };
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

// Constants for gesture detection
/** Maximum duration (ms) for a gesture to be considered a swipe rather than a pan */
const TAP_DURATION_THRESHOLD = 300;
/** Maximum time (ms) between taps to be considered a double-tap */
const DOUBLE_TAP_THRESHOLD = 300;
/** Maximum movement (px) for a gesture to be considered a tap rather than a swipe */
const TAP_MOVEMENT_THRESHOLD = 100;
/** Minimum change in distance between pointers to trigger zoom */
const PINCH_ZOOM_THRESHOLD = 10;

/** Represents a 2D dimension, i.e. a `{x, y}` pair */
class Dimensions {
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
function getDistanceBetweenPoints(p1: Dimensions, p2: Dimensions): number {
  return Math.sqrt(Math.pow(p2.x - p1.x, 2) + Math.pow(p2.y - p1.y, 2));
}

// Helper function to compute the midpoint between two points
function getMidpoint(p1: Dimensions, p2: Dimensions) {
  return new Dimensions((p1.x + p2.x) / 2, (p1.y + p2.y) / 2);
}

// Helper function to limit a number between two values
function limitNumber(min: number, max: number, num: number): number {
  return Math.min(Math.max(num, min), max);
}

/// Relocate the helpDiv element to the top level of the document.
///
/// This is a big hack, but it works. The help div is not in the
/// layout-wrapper, but in the viewer, hence it can't statically be
/// moved to top level. But were we can relocate it before we create
/// the modal.
function relocateHelpDiv(): HTMLElement | null {
  let helpDiv = document.getElementById('helpDiv');
  if (helpDiv != null) {
    helpDiv = document.body.appendChild(helpDiv);
  }
  return helpDiv;
}

/// Helper to download an image.
///
/// Straight out copied from MDN's AI help…
function downloadFile(url: string, fileName: string) {
  // Create an 'a' element
  const a = document.createElement('a');

  // Set the href and download attributes for the anchor element
  a.href = url;
  a.download = fileName || 'defaultFileName';

  // Append the anchor to the document
  document.body.appendChild(a);

  // Trigger the download by simulating click
  a.click();

  // Clean up by removing the element
  document.body.removeChild(a);
}

$(function () {
  const bootdiv = $('#boot');
  const bootinfo = bootdiv.data('view-info');
  const debug = bootdiv.data('debug');
  const LOG = debug ? console.log.bind(console) : function () {};
  const LOG_GROUP = debug ? console.group.bind(console) : function () {};
  const LOG_GROUP_END = debug ? console.groupEnd.bind(console) : function () {};
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const LOG_TABLE = debug ? console.table.bind(console) : function () {};
  const T_START = debug ? console.time.bind(console) : function () {};
  const T_STOP = debug ? console.timeEnd.bind(console) : function () {};
  const ZOOM_FACTOR = 1.1;

  LOG('bootinfo ', bootinfo);

  const cory = new Cory(bootinfo);

  const divMain = $('#main');
  const navMenu = $('#nav');

  const canvas = <HTMLCanvasElement>$('#imageCanvas')[0];
  const context = canvas.getContext('2d');
  const msgBox = $('#messageBox');
  const persistBox = $('#persistBox');
  const moviePlaySpan = $('#moviePlay>span')[0];
  const seekBar = <HTMLInputElement>$('#seekBar')[0];
  const fullScreenIcon = $('#imageFull>span');

  const helpDiv = relocateHelpDiv();

  // Virtual (not-in-DOM) canvas that is used to for pre-rendering
  // images. The alternative would be to use putImageData() instead,
  // and pre-render explicitly the images, tracking said rendering,
  // etc., but this seems much easier.
  const offCanvas = document.createElement('canvas');
  const offContext = offCanvas.getContext('2d');

  const menuToggle = document.getElementById('menuToggle');
  const imageNavGroup = document.getElementById('imageNavGroup');

  if (
    canvas == null ||
    context == null ||
    offCanvas == null ||
    offContext == null ||
    moviePlaySpan == null ||
    seekBar == null ||
    helpDiv == null ||
    menuToggle == null ||
    imageNavGroup == null
  ) {
    LOG('Initialising canvas elements failed!');
    window.alert('Cannot fully initialise the application, aborting!');
    return;
  }

  const helpModal = new bootstrap.Modal(helpDiv, { backdrop: true });

  // Draws an already-loaded image into a give image element.
  function drawImage(
    state: State,
    img: HTMLImageElement,
    info: ImageInfo,
    msg?: string,
    skipStackChange?: boolean,
  ) {
    const url = info.view;
    const transform = info.transform;
    const matrix = info.matrix;
    if (!isImageReady(img)) {
      LOG('Image not ready, re-schedule late loading draw of %s', url);
      img.onload = function () {
        LOG('Late load of ', url);
        setImageReady(img, true);
        drawImage(state, img, info, msg, skipStackChange);
      };
      return;
    }
    if (context == null) {
      LOG('null context?! aborting.');
      return;
    }
    if (!skipStackChange) {
      updateStackVisibility(info);
    }
    LOG_GROUP('Draw image');
    // Reset the canvas transform, clear it, and prepare to draw the (new) image.
    context.setTransform(1, 0, 0, 1, 0, 0);
    context.clearRect(0, 0, canvas.width, canvas.height);

    // Here begin the computations for the image placement.
    /// This is the real canvas size, in bitmap pixels.
    const contextSize = state.canvasSize;
    LOG('transform information: %o matrix information: %o', transform, matrix);
    const rotation = transform[0];
    /** The image size, before any rotation */
    const imgSize = new Dimensions(img.width, img.height);
    /** Image size, taking rotation into account */
    const imgRotated = rotation == 0 ? imgSize : imgSize.swapped();
    // And record the image native size.
    state.imageSize = imgRotated.clone();
    // Compute the absolute (unzoomed) 1:1 scale.
    state.scale11 = imgRotated.dividedBy(contextSize).longest();
    /** The image scale, taking into account any zooming */
    const scale = state.scale11 / cory.state.scale;
    LOG('context %o, image %o, scale: %f', contextSize, imgRotated, scale);
    // Note: target* must be in original coordinate system, not
    // rotated! So using img.width, not imgW. This is because from
    // the point of view of the image, it's drawn straight, not
    // rotated. Sigh, head hurts.
    /** The image target size, after scaling */
    const targetSize = imgSize.scaled(1 / scale);

    LOG(
      'Pre-draw, contextSize: %o imgSize(R=%i): %o imgscaling: %f zoom: %f targetSize: %o',
      contextSize,
      rotation,
      imgRotated,
      scale,
      cory.state.scale,
      targetSize,
    );
    T_START('drawImage');
    // The halved context size in transform then doing the opposite in
    // drawOffsets is required for rotated images. For straight images,
    // this could be skipped, for rotated, it's needed to center the image
    // rotation point, so that rotation is around the center of the
    // screen, not the top-left corner. Otherwise, rotation (e.g. CCW 90)
    // would move the image "on top" of the actual screen.
    const halvedContext = contextSize.scaled(0.5);
    context.transform(
      matrix[0],
      matrix[1],
      matrix[2],
      matrix[3],
      halvedContext.x,
      halvedContext.y,
    );
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
    // Re-check and limit panning to stay within panLimits, but try to
    // scale them based on the limits change, as that keeps a somewhat
    // constant offset zoom position.
    state.panOffsets.x *=
      oldLimits.x !== 0 ? state.panLimits.x / oldLimits.x : 1;
    state.panOffsets.y *=
      oldLimits.y !== 0 ? state.panLimits.y / oldLimits.y : 1;
    state.panOffsets = state.panOffsets.clampLoHi(state.panLimits);
    const finalDrawOffsets = centeringPosition.plus(state.panOffsets);
    LOG(
      'Panning offsets: overflows=%o, centered=%o, pan=%o, offsetDraw=%o',
      overflows,
      centeringPosition,
      state.panOffsets,
      finalDrawOffsets,
    );
    LOG(
      'Cory state: limits %o, offsets %o',
      cory.state.panLimits,
      cory.state.panOffsets,
    );
    context.drawImage(
      img,
      finalDrawOffsets.x,
      finalDrawOffsets.y,
      targetSize.x,
      targetSize.y,
    );
    T_STOP('drawImage');

    // Post-draw actions.
    LOG('post-draw ', url);
    LOG('url: ', url, 'location: ', location.href);
    // Prevent double entries.
    if (url != location.href && url != undefined) {
      history.pushState(info, '', url);
    } else {
      LOG('skipping history pushState', url, location.href);
    }
    cory.state.img = img;
    cory.state.url = url;
    cory.state.transform = transform;
    cory.state.matrix = matrix;
    if (msg != null) {
      writeMessage(msg);
    }
    LOG_GROUP_END();
  }

  function updateInfo(url: string) {
    LOG('Requesting info from ', url);
    $.ajax({
      url: url,
      type: 'GET',
      dataType: 'json',
    }).done(onInfoReceived);
  }

  function redrawImage() {
    drawImage(cory.state, cory.state.img, cory.info.current, undefined, true);
    maybeWriteIsMovie(cory.info.current);
  }

  function incZoom() {
    adjustZoom(ZOOM_FACTOR);
  }

  function decZoom() {
    adjustZoom(1 / ZOOM_FACTOR);
  }

  // Adjust zoom by a factor.
  function adjustZoom(scaleRelative: number) {
    setZoom(cory.state.scale * scaleRelative);
  }

  // Set zoom to a specific value.
  function setZoom(scale: number) {
    // If no-op, just return.
    if (scale == cory.state.scale) {
      return;
    }
    // Check if at initial, or weird state.
    if (scale == 0) {
      scale = 1;
    }
    if (scale <= 1) {
      writeMessage('Minimum zoom reached', 1000);
      scale = 1;
    } else if (scale > 100) {
      writeMessage('Maximum zoom reached', 1000);
      scale = 100;
    }
    cory.state.scale = scale;
    if (cory.state.img.dataset.fullres == 'false') {
      requestFullResImage();
    } else {
      redrawImage();
    }
  }

  // Set zoom to a specific value of pixel ratio. E.g. 1 is 1:1 pixel mapping, 2 is 200%, etc.
  function setPixelZoomRatio(pixelRatio: number) {
    if (cory.state.img.dataset.fullres == 'false') {
      LOG('load later!');
      requestFullResImage(() => setPixelZoomRatio(pixelRatio));
    } else {
      LOG(
        'setting pixel ratio to %f, absolute zoom %f',
        pixelRatio,
        pixelRatio * cory.state.scale11,
      );
      setZoom(pixelRatio * cory.state.scale11);
    }
  }

  function resetZoom() {
    if (cory.state.scale == 1.0) {
      return;
    }
    cory.state.scale = 1.0;
    cory.state.panOffsets = new Dimensions(0, 0);
    redrawImage();
  }

  function requestFullResImage(callback?: () => void) {
    const img = cory.state.img;
    if (img == null || img.dataset.fullres == 'true') {
      return;
    }
    writeMessage('Requesting full screen image…', 2000);
    setImageReady(img, false);
    img.onload = function () {
      LOG('got full size image, calling drawImage');
      setImageReady(img, true);
      const c = cory.info.current;
      drawImage(cory.state, img, c, c.name);
      if (callback != null) {
        callback();
      }
    };
    LOG('Requestiong full size image from', cory.info.current.bytes);
    img.dataset.fullres = 'true';
    img.src = cory.info.current.bytes;
  }

  function resizeCanvas(): boolean {
    if (context == null) {
      // Well, nothing makes sense here, but try to redraw.
      return true;
    }
    // Read the computed (display) dimensions...
    const width = $(context.canvas).width();
    const height = $(context.canvas).height();
    if (width == null || height == null) {
      // Unlikely, but...
      LOG(
        'Resizing canvas, failed to compute w/h, got %o x %o, assuming 300 pixels.',
        width,
        height,
      );
    }
    const scale = window.devicePixelRatio;
    cory.state.devicePixelRatio = scale;
    const scaledWidth = Math.floor((width ?? 300) * scale);
    const scaledHeight = Math.floor((height ?? 300) * scale);
    // to set the model (coordinate) dimension, if changed.
    const newSize = new Dimensions(scaledWidth, scaledHeight);
    if (newSize.equals(cory.state.canvasSize)) {
      LOG('Resizing canvas, no-op.');
      return false;
    }
    LOG(
      'Resizing canvas, display %f x %f, devicePixelRatio %i, scaled %f x %f',
      width,
      height,
      scale,
      scaledWidth,
      scaledHeight,
    );
    // Set the canvas size to the computed dimensions.
    context.canvas.width = scaledWidth;
    context.canvas.height = scaledHeight;
    // And store it in the state.
    cory.state.canvasSize = newSize;
    return true;
  }

  function resizeCanvasAndRedraw() {
    if (resizeCanvas()) redrawImage();
  }

  /// The function that actually loads the image URL/bytes.
  ///
  /// This must be the only place that sets the `src` attribute to
  /// something, to keep the resolution handling abstract.
  function loadImage(img: HTMLImageElement, url: string) {
    setImageReady(img, false);
    img.dataset.fullres = 'false';
    img.src = imageUrlScaled(url);
  }

  function setImageReady(img: HTMLImageElement, done: boolean) {
    $(img).data('done', done);
  }

  function isImageReady(img: HTMLImageElement) {
    return $(img).data('done');
  }

  function handleOffscreenImageLoad(img: HTMLImageElement, kind: string) {
    setImageReady(img, true);
    LOG('Loaded', kind, ' image');
    T_START('post-load');
    // Hack to force pre-rendering. Seems to work, at least in FF
    // and Chrome. For large images on a certain machine, goes
    // from 600ms to ~15 ms (FF), ~0.1ms (Chrome).
    if (offContext != null) {
      offContext.drawImage(img, 0, 0);
    }
    T_STOP('post-load');
  }

  function imageUrlScaled(baseUrl: string): string {
    /** The {@link cory.state.canvasSize} is the size of the canvas in
     * bitmap pixels, so we can directly use it. */
    const maxSide = cory.state.canvasSize.longest();
    const url = new URL(baseUrl);
    url.searchParams.set('res', maxSide.toString());
    return url.toString();
  }

  function requestOffscreenImage(
    img: HTMLImageElement,
    info: ImageInfo | undefined,
    text: string,
  ) {
    if (info != null) {
      img.onload = function () {
        handleOffscreenImageLoad(img, text);
      };
      loadImage(img, info.bytes);
    } else {
      LOG('skipping', text, 'image as unavailable');
    }
  }

  function onInfoReceived(json: ViewInfo) {
    LOG('got cory');
    updateNavbar(json);
    cory.info = json;
    cory.prev = new Image();
    requestOffscreenImage(cory.prev, cory.info.prev, 'prev');
    cory.next = new Image();
    requestOffscreenImage(cory.next, cory.info.next, 'next');
  }

  function enterFullScreen() {
    const div = divMain[0];
    if (screenfull.isEnabled) {
      LOG('entering full screen via screenfull');
      // Sigh, why is the full type not seen correctly?
      (screenfull as screenfull.Screenfull).request(div);
    } else {
      LOG('entering fake full screen');
      cory.state.fullscreen = true;
      navMenu.addClass('nav-hidden');
      divMain.addClass('fake-fullscreen');
      resizeCanvasAndRedraw();
    }
  }

  function leaveFullScreen() {
    if (screenfull.isEnabled) {
      LOG('exiting full screen via screenfull');
      (screenfull as screenfull.Screenfull).exit();
    } else {
      LOG('exiting fake full screen');
      cory.state.fullscreen = false;
      navMenu.removeClass('nav-hidden');
      divMain.removeClass('fake-fullscreen');
      resizeCanvasAndRedraw();
    }
  }

  /**
   * Updates the full screen icon based on the current fullscreen state.
   */
  function updateFullScreenIcon() {
    if (cory.state.fullscreen) {
      fullScreenIcon.removeClass('fa-mazimize');
      fullScreenIcon.addClass('fa-minimize');
    } else {
      fullScreenIcon.removeClass('fa-minimize');
      fullScreenIcon.addClass('fa-mazimize');
    }
  }

  function toggleFullScreen() {
    if (cory.state.fullscreen) {
      leaveFullScreen();
    } else {
      enterFullScreen();
    }
    updateFullScreenIcon();
  }

  // Switches to a non-preloaded image.
  function switchToImage(info: ImageInfo) {
    const image = new Image();
    image.onload = function () {
      setImageReady(image, true);
      drawImage(cory.state, image, info, info.name);
    };
    writeMessage(`Loading ${info.name}...`);
    maybeWriteIsMovie(info);
    loadImage(image, info.bytes);
    updateInfo(info.info);
  }

  function clearMessageAndTimeout() {
    if (cory.state.msgTimeId > 0) {
      window.clearTimeout(cory.state.msgTimeId);
    }
    msgBox.css('visibility', 'hidden');
  }

  function writeMessage(message: string, timeout?: number) {
    clearMessageAndTimeout();
    msgBox.text(message);
    msgBox.css('visibility', 'visible');
    if (timeout == null) {
      timeout = 2000;
    }
    if (timeout > 0) {
      cory.state.msgTimeId = window.setTimeout(function () {
        cory.state.msgTimeId = 0;
        clearMessageAndTimeout();
      }, timeout);
    }
  }
  function writePersistent(message: string) {
    persistBox.text(message);
  }

  function maybeWriteIsMovie(info: ImageInfo) {
    writePersistent(
      info.movie != null
        ? "This is a movie. Press 'p', click or touch to play."
        : '',
    );
  }

  function updateNavbar(topinfo: ViewInfo) {
    $('#navlink1').attr('href', topinfo.yearurl);
    $('#navtext1').text(topinfo.year);
    $('#navlink2').attr('href', topinfo.folderurl);
    $('#navtext2').text(topinfo.folder);
    $('#navlink3').attr('href', topinfo.imageurl);
    $('#navtext3').text(topinfo.image);
  }

  /**
   * Advances to the next image or, in movie move, starts playing the movie.
   */
  function advanceOrPlay() {
    if (cory.info.current.movie != null) {
      launchMovie();
    } else {
      advanceImage(true);
    }
  }

  function advanceImage(forward: boolean) {
    const img = forward ? cory.next : cory.prev;
    const info = forward ? cory.info.next : cory.info.prev;
    if (!info) {
      writeMessage('No ' + (forward ? 'next' : 'previous') + ' image');
      return;
    }
    writeMessage('Loading ' + info.name, 6000);
    maybeWriteIsMovie(info);
    drawImage(cory.state, img, info, info.name);
    updateInfo(info.info);
  }

  function advanceFolder(forward: boolean) {
    const info = forward ? cory.info.nextfolder : cory.info.prevfolder;
    const kind = forward ? 'next folder' : 'previous folder';
    if (!info) {
      writeMessage(`No ${kind} available`);
    } else {
      switchToImage(info);
    }
  }

  function gotoRandomImage() {
    $.ajax({
      url: bootdiv.data('random-url'),
      type: 'GET',
      dataType: 'json',
    }).done(function (json) {
      switchToImage(json.current);
    });
  }

  function gotoFolder() {
    location.href = cory.info.folderurl;
  }

  /** Change display state for elements maching a given class */
  function changeVisibility(className: string, visible: boolean) {
    document.querySelectorAll(className).forEach(function (v) {
      if (v instanceof HTMLElement) {
        v.style.display = visible ? 'block' : 'none';
      }
    });
  }

  /** Event lander for when the movie is ready to play.
   *
   * It hides the (picture) canvas, and shows the video element.
   */
  function movieFrameAvailable(evt: Event) {
    LOG('movie frame available');
    const video = <HTMLVideoElement>evt.target;
    if (video != null) {
      seekBar.disabled = false;
      video.style.visibility = 'visible';
      canvas.style.visibility = 'hidden';
    }
  }

  /** Starts/stops the current video.
   *
   * This triggers loading of the video, if not already loaded (as it
   * is the case on the first play request).
   *
   */
  function launchMovie() {
    if (cory.state.video != null) {
      // Clear out the video message, as once the user starts interacting
      // with the video, it's no longer relevant.
      writePersistent('');
      if (cory.state.video.paused) {
        cory.state.video.play();
        moviePlaySpan.classList.remove('fa-play');
        moviePlaySpan.classList.add('fa-pause');
      } else {
        cory.state.video.pause();
        moviePlaySpan.classList.remove('fa-pause');
        moviePlaySpan.classList.add('fa-play');
      }
    }
  }

  /** Drops the current video, if any.
   *
   * This is used to clean up the video element, such that in-flight load
   * requests are cancelled, and later loads won't clobber a new image,
   * in case we navigated away from the video, even if to a new video).
   *
   */
  function dropCurrentVideo() {
    if (cory.state.video != null) {
      cory.state.video.pause();
      cory.state.video.src = '';
      cory.state.video.remove();
      cory.state.video = null;
    }
  }

  /** Switches between image and video mode.
   *
   * This is not entirely straightforward. On navigating to image mode, the
   * current video is dropped, thus cancelling any pending in-flight load
   * requests, and the new image is immediately shown (via making the canvas
   * visible). On navigating to video mode, the video element is created, but
   * not preloaded, so the (video) static image is shown until the user
   * initiates the play action, and further, is kept visible until the video
   * is loaded and ready to play.
   *
   * @param info the current image info
   */
  function updateStackVisibility(info: ImageInfo) {
    // First, show the canvas, and hide the video. The reason why is because
    // this function is only ever called from drawImage(), which will draw
    // either the (real) image or the video poster image on the canvas anyway.
    // And if the canvas is not shown, switching between videos (by chance or
    // based on filtering) the output will be a white page.
    //
    // Note we don't set the controls to picture, just the canvas/video
    // visibility.
    canvas.style.visibility = 'visible';
    seekBar.style.visibility = 'hidden';
    if (info.movie != null) {
      LOG('loading movie and prepare to switch to movie mode');
      dropCurrentVideo();
      const video = document.createElement('video');
      cory.state.video = video;
      // Don't load the video by default, to keep the UI fast and traffic low.
      video.setAttribute('preload', 'none');
      //video.setAttribute('poster', imageUrlScaled(info.bytes));
      video.classList.add('viewer-video');
      video.onloadeddata = movieFrameAvailable;
      const source = document.createElement('source');
      source.setAttribute('src', info.movie);
      video.appendChild(source);
      divMain.append(video);
      seekBar.style.visibility = 'visible';
      seekBar.disabled = true;
      changeVisibility('.nav-only-image', false);
      changeVisibility('.nav-only-video', true);
      seekBar.valueAsNumber = 0;
      video.addEventListener('timeupdate', function () {
        const value = (100 / video.duration) * video.currentTime;
        seekBar.valueAsNumber = value;
      });
    } else {
      LOG('switching to picture mode');
      changeVisibility('.nav-only-image', true);
      changeVisibility('.nav-only-video', false);
      dropCurrentVideo();
    }
  }

  function flagImage(flag: boolean) {
    $.ajax({
      url: cory.info.current.flag,
      type: flag ? 'PUT' : 'DELETE',
      dataType: 'json',
    })
      .done(function (json) {
        writeMessage(json.text, 2000);
      })
      .fail(function (xhr, status, details) {
        writeMessage(
          'Error flagging image: ' + status + ', details: ' + details,
        );
      });
  }

  function setupTouchAndGestureHandlers() {
    // Track pointer start position and time for gesture detection
    const pointerStart = new Dimensions(0, 0);
    let pointerStartTime = 0;
    let lastTapTime = 0;
    let pointerId = -1;

    // Add pinch-zoom tracking
    const activePointers = new Map();
    let initialPinchDistance = 0;
    let prevCenter = new Dimensions(0, 0);
    LOG('X: setupTouchAndGestureHandlers()');

    // Handle pointer down
    canvas.addEventListener(
      'pointerdown',
      function (e) {
        LOG('X: pointerdown ' + e.pointerId);

        // Store this pointer
        activePointers.set(e.pointerId, new Dimensions(e.clientX, e.clientY));

        // If this is the first or only pointer, track for swipe/tap
        if (activePointers.size === 1) {
          pointerId = e.pointerId;
          pointerStart.x = e.clientX;
          pointerStart.y = e.clientY;
          pointerStartTime = Date.now();
        }
        // If this is the second pointer, initialize pinch-zoom
        else if (activePointers.size === 2) {
          const pointers = Array.from(activePointers.values());
          initialPinchDistance = getDistanceBetweenPoints(
            pointers[0],
            pointers[1],
          );
          prevCenter = getMidpoint(pointers[0], pointers[1]);
          LOG('X: pinch start, initial distance: ' + initialPinchDistance);
        }

        // Always capture the pointer
        canvas.setPointerCapture(e.pointerId);

        // Prevent default to stop Safari's native zoom
        if (e.pointerType === 'touch') {
          e.preventDefault();
        }
      },
      { passive: false },
    );

    // Handle pointer move for pinch-zoom
    canvas.addEventListener(
      'pointermove',
      function (e) {
        // Handle the simplest and most common case.
        if (activePointers.size === 0) return;
        // After this, there's at least one pointer down.
        const prevPointer = Array.from(activePointers.values())[0];
        const pointer = new Dimensions(e.clientX, e.clientY);
        const delta = pointer.minus(prevPointer);
        const minimumMove = cory.state.devicePixelRatio;
        // If this pointer hasn't moved at least 1px, ignore the event
        // entirely. The movement will accumulate, and a series of small
        // movements should be reflected once it jumps enough.
        if (delta.absMagnitude() < minimumMove) {
          return;
        }
        // Update this pointer position.
        if (activePointers.has(e.pointerId)) {
          activePointers.set(e.pointerId, pointer);
        }
        switch (activePointers.size) {
          case 1: {
            // Handle panning if we have exactly 1 pointer.
            if (cory.pan(delta.x, delta.y)) {
              redrawImage();
            }
            break;
          }
          case 2: {
            // Handle pinch-zoom and pan if we have exactly 2 pointers.
            const pointers = Array.from(activePointers.values());
            const currentDistance = getDistanceBetweenPoints(
              pointers[0],
              pointers[1],
            );

            // Calculate current pinch center point.
            const center = getMidpoint(pointers[0], pointers[1]);

            // Calculate movement delta (how much the center moved)
            const centerDelta = center.minus(prevCenter);

            // Only process if we've moved enough to consider it intentional
            const hasPanned =
              Math.abs(centerDelta.x) > minimumMove ||
              Math.abs(centerDelta.y) > minimumMove;
            const hasZoomed =
              Math.abs(currentDistance - initialPinchDistance) >
              PINCH_ZOOM_THRESHOLD;
            LOG('PinchPan, hasPanned=%o, hasZoomed=%o', hasPanned, hasZoomed);
            if (hasPanned || hasZoomed) {
              // Apply pan - move against the finger direction
              let panDidMove = false;
              if (hasPanned) {
                panDidMove = cory.pan(centerDelta.x, centerDelta.y);

                LOG(
                  'X: panning by %o, new origin %o',
                  centerDelta,
                  cory.state.panOffsets,
                );
              }

              // Apply zoom if distance changed
              if (hasZoomed) {
                // Calculate new scale
                const scaleFactor = currentDistance / initialPinchDistance;
                LOG(
                  'X: pinch zoom, scale factor %f, around center %o',
                  scaleFactor,
                  center,
                );

                // Apply zoom
                adjustZoom(scaleFactor);

                // Update initial distance for smoother zooming
                initialPinchDistance = currentDistance;
              }

              // Save current center as previous for next move
              prevCenter = center;

              // Only redraw once
              if (!hasZoomed && panDidMove) {
                redrawImage();
              }
            }

            // Prevent default to stop Safari's native zoom
            if (e.pointerType === 'touch') {
              e.preventDefault();
            }
          }
        }
      },
      { passive: false },
    );

    // Handle pointer up for gesture detection
    canvas.addEventListener('pointerup', function (e) {
      LOG('X: pointerup ' + e.pointerId);

      // Remove this pointer
      activePointers.delete(e.pointerId);

      // Only process tap/swipe if this was the initial pointer
      if (e.pointerId === pointerId) {
        // Track pointer end position and time for gesture detection
        const pointerEndTime = Date.now();
        const pointerDuration = pointerEndTime - pointerStartTime;
        const pointerEnd = new Dimensions(e.clientX, e.clientY);
        const delta = pointerEnd.minus(pointerStart);
        const totalMovement = Math.sqrt(delta.x * delta.x + delta.y * delta.y);

        LOG(
          'X: pointerup ' +
            e.pointerId +
            ' duration ' +
            pointerDuration +
            ' movement ' +
            totalMovement,
        );
        // First check for short touches (mostly swipes)
        if (pointerDuration < TAP_DURATION_THRESHOLD) {
          // Check if swipe (non-trivial movement)
          if (totalMovement > TAP_MOVEMENT_THRESHOLD) {
            // But make sure we're not in zoomed mode, in which case the
            // action is pan, and was handled in the 'pointermove' event
            // handler.
            if (cory.state.scale === 1.0) {
              // Check if regular horizontal swipe
              if (Math.abs(delta.x) > Math.abs(delta.y)) {
                if (delta.x > 0) {
                  // Right swipe
                  LOG('swipe right detected');
                  advanceImage(false);
                } else {
                  // Left swipe
                  LOG('swipe left detected');
                  advanceImage(true);
                }
              } else {
                // This is a no movement, or vertical movement swipe.
                // TODO: what do do with vertical swipes?
                LOG('Ignored vertical swipe');
              }
            }
          } else {
            // Check for double tap.
            const tapTimeDiff = pointerEndTime - lastTapTime;
            if (tapTimeDiff < DOUBLE_TAP_THRESHOLD) {
              // This is a double tap
              LOG('X: double tap detected');
              toggleFullScreen();
              lastTapTime = 0; // Reset to prevent triple tap detection
            } else {
              // This is a single tap
              LOG('X: single tap detected, launching movie');
              launchMovie();
              lastTapTime = pointerEndTime;
            }
          }
        } else {
          // This is a pan _end_. However, we should have handled the
          // actual pan during the pointermove events, so nothing to do
          // here.
        }
      }
    });

    // Handle pointer cancel to clean up state
    canvas.addEventListener('pointercancel', function (e) {
      LOG('X: pointercancel ' + e.pointerId);
      activePointers.delete(e.pointerId);
      if (e.pointerId === pointerId) {
        pointerId = -1;
      }
    });

    // Prevent all gesture events at the document level to stop Safari's
    // behavior.
    // TODO: enable gesture handling, for example trackpad
    // pinch/zoom, which is different than touch pinch-zoom. Sigh.
    document.addEventListener(
      'gesturestart',
      function (e) {
        LOG('X: gesturestart prevented');
        e.preventDefault();
      },
      { passive: false },
    );

    document.addEventListener(
      'gesturechange',
      function (e) {
        LOG('X: gesturechange prevented');
        e.preventDefault();
      },
      { passive: false },
    );

    document.addEventListener(
      'gestureend',
      function (e) {
        LOG('X: gestureend prevented');
        e.preventDefault();
      },
      { passive: false },
    );

    // Prevent default for touch pointers to avoid browser handling conflicts
    canvas.addEventListener(
      'pointerdown',
      function (e) {
        if (e.pointerType === 'touch') {
          e.preventDefault();
        }
      },
      { passive: false },
    );
  }
  setupTouchAndGestureHandlers();
  /// Toggles the help div.
  function toggleHelp() {
    helpModal.toggle();
  }

  /// Toggles between fit and 100% pixel ratios.
  function toggleZoomFit() {
    if (cory.state.scale == 1) setPixelZoomRatio(1);
    else setZoom(1);
  }

  document.addEventListener('keydown', function (e) {
    // Ignore pressses of just the modifier key.
    if (
      e.key === 'Shift' ||
      e.key === 'Control' ||
      e.key === 'Alt' ||
      e.key === 'Meta'
    ) {
      return;
    }
    const active = document.activeElement;
    if (active != null && active.id === 'entry') {
      return;
    }
    let handled = true;
    LOG('key: %o, full event: %o', e.key, e);

    // Handle the modal first, as it's a special case. Any key press here
    // closes the modal, and does nothing else (and doesn't propagate).
    if (helpDiv.classList.contains('show')) {
      helpModal.hide();
      e.preventDefault();
      return;
    }

    // Currently we don't handle complex key shortcuts, so if any modifier
    // key is pressed, return. Shift is special as it's used for capital
    // letters and reflects in the key value, so we don't check for it.
    // And Command/Control is handled separately, so basically only
    // Alt/Meta is ignored.
    if (e.metaKey) {
      LOG('Ignoring key with meta modifier');
      return;
    }
    if (e.altKey) {
      handled = true;
      // TODO: make the modifier a relative size, not absolute. Needs
      // plugging in to canvas size.
      const modifier = e.shiftKey ? 50 : 25;
      let didPan = false;
      switch (e.key) {
        case 'ArrowUp':
          didPan = cory.panY(-modifier);
          break;
        case 'ArrowDown':
          didPan = cory.panY(modifier);
          break;
        case 'ArrowLeft':
          didPan = cory.panX(-modifier);
          break;
        case 'ArrowRight':
          didPan = cory.panX(modifier);
          break;
        default:
          handled = false;
          break;
      }
      if (handled) {
        if (didPan) {
          redrawImage();
        }
        e.preventDefault();
        return;
      }
    }
    switch (e.key) {
      case '+':
      case '=':
        incZoom();
        break;
      case '-':
        decZoom();
        break;
      case '0':
        resetZoom();
        break;
      case '1':
        setPixelZoomRatio(1);
        break;
      case '2':
        setPixelZoomRatio(2);
        break;
      case '3':
        setPixelZoomRatio(3);
        break;
      case '4':
        setPixelZoomRatio(4);
        break;
      case 'f':
        toggleFullScreen();
        break;
      case 'p':
        launchMovie();
        break;
      case 'r':
      case 'Tab':
        gotoRandomImage();
        break;
      case 'u':
        gotoFolder();
        break;
      case 'x':
        flagImage(true);
        break;
      case 'n':
        flagImage(false);
        break;
      case 'b':
        triggerBrowseMode();
        break;
      case 'l':
        triggerListMode();
        break;
      case 'z':
        toggleZoomFit();
        break;
      case 'B':
        triggerFolderBrowseMode();
        break;
      case 'L':
        triggerFolderListMode();
        break;
      case 'D':
        triggerImageDownload();
        break;
      case 'UpArrow':
        toggleFullScreen();
        break;
      case 'ArrowLeft':
        advanceImage(false);
        break;
      case 'ArrowRight':
        advanceImage(true);
        break;
      case ' ':
        advanceOrPlay();
        break;
      case '[':
      case 'PageUp':
        advanceFolder(false);
        break;
      case ']':
      case 'PageDown':
        advanceFolder(true);
        break;
      case 'Home':
        switchToImage(cory.info.first);
        break;
      case 'End':
        switchToImage(cory.info.last);
        break;
      case 'Backspace':
      case 'Delete':
      case '<':
        // Allow these two keys to navigate backwards. Useful in
        // full-screen mode.
        window.history.back();
        break;
      case '>':
        // Allow this to navigate forwards. Useful in full-screen mode, to
        // provide parity with backwards.
        window.history.forward();
        break;
      case '.':
      case 'm':
        menuToggle.click();
        break;
      case '?':
      case 'h':
        toggleHelp();
        break;
      default:
        handled = false;
        break;
    }
    if (handled) {
      e.preventDefault();
    }
  });

  document.addEventListener('fullscreenchange', function () {
    cory.state.fullscreen = document.fullscreenElement != null;
    updateFullScreenIcon();
    resizeCanvasAndRedraw();
  });

  // Handle forward/back buttons. If a state has been provided, we jump
  // back to that state (as an ImageInfo).
  window.addEventListener('popstate', (event) => {
    LOG('popstate event: ', event.state);
    if (event.state) {
      // Simulate the loading of the previous page
      switchToImage(event.state);
    }
  });

  menuToggle.addEventListener('hidden.bs.dropdown', function () {
    imageNavGroup.classList.remove('revealed');
  });
  menuToggle.addEventListener('shown.bs.dropdown', function () {
    imageNavGroup.classList.add('revealed');
  });

  function triggerImageDownload() {
    downloadFile(cory.info.current.bytes, cory.info.current.name);
  }

  function triggerBrowseMode() {
    window.location.href = cory.info.current.browse;
  }

  function triggerListMode() {
    window.location.href = cory.info.current.list;
  }

  function triggerFolderBrowseMode() {
    window.location.href = cory.info.folderbrowse;
  }

  function triggerFolderListMode() {
    window.location.href = cory.info.folderlist;
  }

  // Based on current layout, convert the div#main to fixed
  // position, keeping - hopefully - same absolute location inside
  // the window.
  function mainToFixed() {
    // Hide footer.
    $('footer').css('display', 'none');
    // For viewer app, we want the divmain to be fixed-viewport, and not
    // grow, as it is for normal "text" pages.
    divMain.css({ overflow: 'hidden', position: 'relative' });
    // Also remove container-fluid, as here we want as much display space
    // as possible.
    divMain.removeClass('container-fluid');
    $('#imageFull').on('click', function () {
      toggleFullScreen();
    });
    $('#imageRand').on('click', function () {
      gotoRandomImage();
    });
    $('#goImageInfo').on('click', function () {
      window.location.href = cory.info.imageurl;
    });
    $('#downloadImage').on('click', triggerImageDownload);
    $('#goParent').on('click', gotoFolder);
    $('#goBrowse').on('click', triggerBrowseMode);
    $('#goList').on('click', triggerListMode);
    $('#goFolderBrowse').on('click', triggerFolderBrowseMode);
    $('#goFolderList').on('click', triggerFolderListMode);
    $('#imageZoom').on('click', toggleZoomFit);
    $('#imagePrev').on('click', function () {
      advanceImage(false);
    });
    $('#imageNext').on('click', function () {
      advanceImage(true);
    });
    $('#folderPrev').on('click', function () {
      advanceFolder(false);
    });
    $('#folderNext').on('click', function () {
      advanceFolder(true);
    });

    // movie-specific controls
    $('#moviePlay').on('click', function () {
      launchMovie();
    });

    seekBar.addEventListener('input', function () {
      if (cory.state.video != null) {
        const time = cory.state.video.duration * (seekBar.valueAsNumber / 100);
        cory.state.video.currentTime = time;
      }
    });
  }

  mainToFixed();
  resizeCanvas();
  maybeWriteIsMovie(bootinfo.current);

  const image = new Image();
  image.onload = function () {
    setImageReady(image, true);
    const c = bootinfo.current;
    drawImage(cory.state, image, c, c.name);
  };
  loadImage(image, bootinfo.current.bytes);

  // Process the rest of info (load prev/next images) only after the
  // current image loading has been triggered, for faster startup.
  onInfoReceived(bootinfo);

  if ('ResizeObserver' in window) {
    const resizeObserver = new ResizeObserver((entries) => {
      for (const entry of entries) {
        if (entry.target === canvas) {
          resizeCanvasAndRedraw();
          break;
        }
      }
    });
    resizeObserver.observe(canvas);
  } else {
    // Fallback to old-style window.resize, but as I see, any platform
    // post 2020 should support the resize observer.
    (window as Window).addEventListener('resize', resizeCanvasAndRedraw, false);
  }
});
