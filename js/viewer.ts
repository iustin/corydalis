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
  scale: number;
  originX: number;
  originY: number;
};

type Cory = {
  info: ViewInfo;
  prev: HTMLImageElement;
  next: HTMLImageElement;
  state: State;
};

// Constants for gesture detection
/** Maximum duration (ms) for a gesture to be considered a swipe rather than a pan */
const SWIPE_DURATION_THRESHOLD = 300;
/** Maximum time (ms) between taps to be considered a double-tap */
const DOUBLE_TAP_THRESHOLD = 300;
/** Maximum movement (px) for a gesture to be considered a tap rather than a swipe */
const TAP_MOVEMENT_THRESHOLD = 10;
/** Minimum distance (px) for a gesture to be considered a swipe */
const SWIPE_DISTANCE_THRESHOLD = 100;

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

  /** Returns the longest dimension */
  public longest(): number {
    return this.x > this.y ? this.x : this.y;
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
  const T_START = debug ? console.time.bind(console) : function () {};
  const T_STOP = debug ? console.timeEnd.bind(console) : function () {};
  const ZOOM_FACTOR = 1.1;

  LOG('bootinfo ', bootinfo);

  const cory: Cory = {
    info: bootinfo,
    prev: new Image(),
    next: new Image(),
    state: {
      fullscreen: false,
      img: new Image(),
      video: null,
      lastX: 0,
      msgTimeId: 0,
      transform: bootinfo.current.transform,
      matrix: bootinfo.current.matrix,
      url: location.href,
      scale: 1.0,
      originX: 0.5,
      originY: 0.5,
    },
  };

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
        drawImage(img, info, msg, skipStackChange);
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

    // Reset the canvas transform, clear it, and prepare to draw the (new) image.
    context.setTransform(1, 0, 0, 1, 0, 0);
    context.clearRect(0, 0, canvas.width, canvas.height);
    /** Used for high-res display support */
    const deviceScale = window.devicePixelRatio;
    // The default values: unlikely to be used, as we pass a DOM object, but TS...
    /** Scale the given value based on the device scale. */
    const contextScaler = (val: number | undefined): number =>
      Math.floor((val ?? 300) * deviceScale);
    /** Context size, modified for high-res scaling */
    const contextSize = new Dimensions(
      contextScaler($(context.canvas).width()),
      contextScaler($(context.canvas).height()),
    );
    const zoomedContext = contextSize.scaled(cory.state.scale);
    LOG('transform information: %o matrix information: %o', transform, matrix);
    const rotation = transform[0];
    const imgSize = new Dimensions(img.width, img.height);
    /** Image size, taking rotation into account */
    const imgRotated = rotation == 0 ? imgSize : imgSize.swapped();
    // Compute the potentially-zoomed scaling of the image.
    const imgScaling = imgRotated.dividedBy(zoomedContext);
    const scale = imgScaling.longest();
    // Note: target* must be in original coordinate system, not
    // rotated! So using img.width, not imgW. This is because from
    // the point of view of the image, it's drawn straight, not
    // rotated. Sigh, head hurts.
    const targetSize = imgSize.scaled(1 / scale);
    // Compute the draw offsets, to center the image, if it's smaller than
    // the canvas. Note this is in a positive coordinates system, not in
    // the (0, 0) being the center of the screen. Later we subtract the
    // halvedCanvas to align it with the actual canvas coordinates.
    const drawOffsets = contextSize
      .minus(targetSize)
      .scaled(1 / 2)
      .clampMin(0);
    LOG(
      'pre-draw, contextSize: %o zoomedContext: %o imgSize(R=%i): %o imgScaling: %o scale: %f zoom: %f targetSize: %o offsets: %o',
      contextSize,
      zoomedContext,
      rotation,
      imgRotated,
      imgScaling,
      scale,
      cory.state.scale,
      targetSize,
      drawOffsets,
    );
    cory.state.lastX = drawOffsets.x;
    LOG('matrix: %o', matrix);
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
    const offsetDrawOffsets = drawOffsets.minus(halvedContext);
    LOG(
      'draw call: adjustedDrawOffsets: %o targetSize: %o',
      offsetDrawOffsets,
      targetSize,
    );
    context.drawImage(
      img,
      offsetDrawOffsets.x,
      offsetDrawOffsets.y,
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
    drawImage(cory.state.img, cory.info.current, undefined, true);
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

  function resetZoom() {
    cory.state.scale = 1.0;
    cory.state.originX = 0.5;
    cory.state.originY = 0.5;
    redrawImage();
  }

  function requestFullResImage() {
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
      drawImage(img, c, c.name);
    };
    LOG('Requestiong full size image from', cory.info.current.bytes);
    img.dataset.fullres = 'true';
    img.src = cory.info.current.bytes;
  }

  function resizeCanvas() {
    if (context == null) {
      return;
    }
    // Reset main div top position, in case navbar changed size.
    divMain.css({ top: computeNavBarHeight() });
    // Read the computed (display) dimensions...
    const width = $(context.canvas).width();
    const height = $(context.canvas).height();
    if (width == null || height == null) {
      // Unlikely, but...
      LOG(
        'Resizing canvas, failed to compute w/h, width: ',
        width,
        ', height: ',
        height,
        ', aborting.',
      );
      return;
    }
    const scale = window.devicePixelRatio;
    const scaledWidth = Math.floor(width * scale);
    const scaledHeight = Math.floor(height * scale);
    LOG(
      'Resizing canvas, width ',
      width,
      ', height ',
      height,
      'scaled: ',
      scaledWidth,
      ' x ',
      scaledHeight,
    );
    // to set the model (coordinate) dimension.
    context.canvas.width = scaledWidth;
    context.canvas.height = scaledHeight;
  }

  function resizeCanvasAndRedraw() {
    resizeCanvas();
    redrawImage();
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
    const w = $(canvas).width() ?? 300;
    const h = $(canvas).height() ?? 300;
    const scale = window.devicePixelRatio;
    // LOG('Scale is ', scale);
    const r = Math.floor((w > h ? w : h) * scale);
    // const r = w > h ? w : h;
    // TODO: suply and read rendered sizes in/from boot data, and
    // make calls only for the right image sizes.
    const url = new URL(baseUrl);
    url.searchParams.set('res', r.toString());
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
      drawImage(image, info, info.name);
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
    drawImage(img, info, info.name);
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
    let pointerStartX = 0;
    let pointerStartY = 0;
    let pointerStartTime = 0;
    let lastTapTime = 0;
    let pointerId = -1;
    LOG('X: setupTouchAndGestureHandlers()');

    // Handle pointer down
    canvas.addEventListener('pointerdown', function (e) {
      LOG('X: pointerdown');
      pointerId = e.pointerId;
      pointerStartX = e.clientX;
      pointerStartY = e.clientY;
      pointerStartTime = Date.now();

      // Capture pointer to ensure we get all events
      canvas.setPointerCapture(e.pointerId);
    });

    // Handle pointer up for gesture detection
    canvas.addEventListener('pointerup', function (e) {
      // Only process if it's the same pointer that started the gesture
      if (e.pointerId !== pointerId) return;

      LOG('X: pointerup');
      const pointerEndTime = Date.now();
      const pointerDuration = pointerEndTime - pointerStartTime;
      const pointerEndX = e.clientX;
      const pointerEndY = e.clientY;
      const deltaX = pointerEndX - pointerStartX;
      const deltaY = pointerEndY - pointerStartY;
      const totalMovement = Math.sqrt(deltaX * deltaX + deltaY * deltaY);

      // First check for swipe - fast movement with significant horizontal distance
      if (
        pointerDuration < SWIPE_DURATION_THRESHOLD &&
        Math.abs(deltaX) > SWIPE_DISTANCE_THRESHOLD &&
        Math.abs(deltaX) > Math.abs(deltaY)
      ) {
        if (deltaX > 0) {
          // Right swipe
          LOG('swipe right detected');
          advanceImage(false);
        } else {
          // Left swipe
          LOG('swipe left detected');
          advanceImage(true);
        }
        return;
      }

      // If not a swipe, check for tap (minimal movement)
      if (totalMovement < TAP_MOVEMENT_THRESHOLD) {
        // Check for double tap
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
    });

    // Handle pointer cancel to clean up state
    canvas.addEventListener('pointercancel', function (e) {
      if (e.pointerId === pointerId) {
        pointerId = -1;
      }
    });

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
    LOG("key: '", e.key, "'");

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
    if (e.altKey || e.metaKey) {
      return;
    }
    if (e.ctrlKey) {
      handled = true;
      switch (e.key) {
        case 'ArrowUp':
          cory.state.originY -= 0.1;
          break;
        case 'ArrowDown':
          cory.state.originY += 0.1;
          break;
        case 'ArrowLeft':
          cory.state.originX -= 0.1;
          break;
        case 'ArrowRight':
          cory.state.originX += 0.1;
          break;
        default:
          handled = false;
          break;
      }
      if (handled) {
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

  function computeNavBarHeight(): number {
    if (cory.state.fullscreen) {
      return 0;
    } else {
      const navbar = $('nav.navbar');
      return navbar.outerHeight() ?? 0;
    }
  }

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
    // Remove container-fluid, as here we want as much display
    // space as possible.
    divMain.removeClass('container-fluid');
    // Compute current location (on screen), based on navbar
    // height.
    const navBarH = computeNavBarHeight();
    LOG('navbar at ', navBarH);
    // And convert to absolute at same location.
    divMain.css({
      top: navBarH,
      position: 'fixed',
      bottom: 0,
      left: 0,
      right: 0,
    });
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
    $('#imageZoom').on('click', function () {
      requestFullResImage();
    });
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
    drawImage(image, c, c.name);
  };
  loadImage(image, bootinfo.current.bytes);

  // Process the rest of info (load prev/next images) only after the
  // current image loading has been triggered, for faster startup.
  onInfoReceived(bootinfo);

  window.addEventListener('resize', resizeCanvasAndRedraw, false);
  window.addEventListener('orientationchange', resizeCanvasAndRedraw, false);
});
