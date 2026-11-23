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

// The image viewer: connects the page (canvas, controls, keyboard and
// pointer events) to the viewer's logic (js/viewer-logic.ts). The window
// and the browser-only libraries (Bootstrap's modal, screenfull) are
// passed in by js/viewer.ts, so that the tests can run this with jsdom.

// With the extensions, as the unit tests run this file directly in Node.
import { bootData, bootJSON } from './boot.ts';
import { flagErrorMessage, sendFlag } from './flag.ts';
import { type RequestEnv, getJSON, requestFailureMessage } from './http.ts';
import { playFailureMessage } from './media.ts';
import {
  Cory,
  Dimensions,
  type ImageInfo,
  PINCH_ZOOM_THRESHOLD,
  type State,
  type ViewInfo,
  ZOOM_FACTOR,
  classifyPointerUp,
  clampZoom,
  getDistanceBetweenPoints,
  getMidpoint,
  imageUrlScaled,
  layoutImage,
  movieMessage,
  seekPercent,
  seekTime,
  wheelZoomRatio,
} from './viewer-logic.ts';

/** The help dialog: the parts of Bootstrap's Modal used by the viewer. */
export type HelpModal = {
  toggle(): void;
  hide(): void;
};

/** The full screen API: the parts of screenfull used by the viewer. */
export type FullScreenApi = {
  readonly isEnabled: boolean;
  readonly isFullscreen: boolean;
  request(element: Element): Promise<void>;
  exit(): Promise<void>;
};

/** The window the viewer runs in, with its DOM classes. */
export type ViewerWindow = Window & typeof globalThis;

/** The browser-only dependencies of the viewer. */
export type ViewerDeps = {
  /** Creates the help dialog (Bootstrap's Modal) for the help element. */
  helpModal: (element: HTMLElement) => HelpModal;
  /** The full screen API (screenfull). */
  fullScreen: FullScreenApi;
  /** Options for the requests to the server; replaceable in the tests. */
  requestEnv?: RequestEnv;
  /** Goes to another page; defaults to setting the window's location. */
  navigate?: (url: string) => void;
};

/// Relocate the helpDiv element to the top level of the document.
///
/// This is a big hack, but it works. The help div is not in the
/// layout-wrapper, but in the viewer, hence it can't statically be
/// moved to top level. But were we can relocate it before we create
/// the modal.
export function relocateHelpDiv(doc: Document): HTMLElement | null {
  let helpDiv = doc.getElementById('helpDiv');
  if (helpDiv != null) {
    helpDiv = doc.body.appendChild(helpDiv);
  }
  return helpDiv;
}

/// Helper to download an image.
///
/// Straight out copied from MDN's AI help…
export function downloadFile(doc: Document, url: string, fileName: string) {
  // Create an 'a' element
  const a = doc.createElement('a');

  // Set the href and download attributes for the anchor element
  a.href = url;
  a.download = fileName || 'defaultFileName';

  // Append the anchor to the document
  doc.body.appendChild(a);

  // Trigger the download by simulating click
  a.click();

  // Clean up by removing the element
  doc.body.removeChild(a);
}

/**
 * Starts the viewer in the given (loaded) window. Returns the viewer's
 * state, or null if the page lacks the image info or the viewer's
 * elements (after telling the user).
 */
export function startViewer(win: ViewerWindow, deps: ViewerDeps): Cory | null {
  const document = win.document;
  const location = win.location;
  const history = win.history;
  const screenfull = deps.fullScreen;
  const requestEnv = deps.requestEnv ?? {};
  const navigate =
    deps.navigate ??
    ((url: string) => {
      location.href = url;
    });
  const bootinfo = bootJSON<ViewInfo>('view-info', document);
  const debug = bootJSON<boolean>('debug', document) ?? false;
  const LOG = debug ? console.log.bind(console) : function () {};
  const LOG_GROUP = debug ? console.group.bind(console) : function () {};
  const LOG_GROUP_END = debug ? console.groupEnd.bind(console) : function () {};
  const T_START = debug ? console.time.bind(console) : function () {};
  const T_STOP = debug ? console.timeEnd.bind(console) : function () {};

  LOG('bootinfo ', bootinfo);
  if (bootinfo == null) {
    win.alert('Cannot initialise the application (no image info), aborting!');
    return null;
  }

  const cory = new Cory(bootinfo, {
    url: location.href,
    devicePixelRatio: win.devicePixelRatio,
    newImage: () => new win.Image(),
  });

  // The elements of the page; checked for existence below.
  const divMain = document.getElementById('main') as HTMLElement;
  const navMenu = document.getElementById('nav') as HTMLElement;

  const canvas = document.getElementById('imageCanvas') as HTMLCanvasElement;
  const context = canvas?.getContext('2d');
  const msgBox = document.getElementById('messageBox') as HTMLElement;
  const persistBox = document.getElementById('persistBox') as HTMLElement;
  const moviePlaySpan = document.querySelector(
    '#moviePlay>span',
  ) as HTMLElement;
  const seekBar = document.getElementById('seekBar') as HTMLInputElement;
  const fullScreenIcon = document.querySelector(
    '#imageFull>span',
  ) as HTMLElement;

  const helpDiv = relocateHelpDiv(document);

  /** Images that have finished loading, see setImageReady. */
  const readyImages = new WeakSet<HTMLImageElement>();

  /** Attaches a click handler to an element, if it exists. */
  function onClick(id: string, handler: () => void) {
    document.getElementById(id)?.addEventListener('click', handler);
  }

  // Virtual (not-in-DOM) canvas that is used to for pre-rendering
  // images. The alternative would be to use putImageData() instead,
  // and pre-render explicitly the images, tracking said rendering,
  // etc., but this seems much easier.
  const offCanvas = document.createElement('canvas');
  const offContext = offCanvas.getContext('2d');

  const menuToggle = document.getElementById('menuToggle');
  const imageNavGroup = document.getElementById('imageNavGroup');
  const imageInfo = document.getElementById('imageInfo');
  const imageInfoText = document.getElementById('imageInfoText');
  const waitingBar = document.getElementById('waiting-bar');

  if (
    divMain == null ||
    navMenu == null ||
    msgBox == null ||
    persistBox == null ||
    fullScreenIcon == null ||
    canvas == null ||
    context == null ||
    offCanvas == null ||
    offContext == null ||
    moviePlaySpan == null ||
    seekBar == null ||
    helpDiv == null ||
    menuToggle == null ||
    imageNavGroup == null ||
    waitingBar == null ||
    imageInfo == null ||
    imageInfoText == null
  ) {
    LOG('Initialising canvas elements failed!');
    win.alert('Cannot fully initialise the application, aborting!');
    return null;
  }
  const helpModal = deps.helpModal(helpDiv);

  // Now that the base setup has been done, store the current image state,
  // so that navigating back to this initial image is possible. Other new
  // images are handled by pushState in drawImage(), but for this initial
  // image, drawImage will not save state as it is as the initial (same)
  // URL.
  history.replaceState(cory.info.current, '');

  // The functions below need '!' for some of the elements checked above
  // (e.g. context, waitingBar): function declarations are hoisted, and
  // could be called before the null checks, so TypeScript doesn't apply
  // them there. canvas doesn't, since it's declared with a type
  // assertion (as HTMLCanvasElement), which already excludes null.

  // Draws an already-loaded image into a give image element.
  function drawImage(
    state: State,
    img: HTMLImageElement,
    info: ImageInfo,
    options?: {
      showName?: boolean;
      skipStackChange?: boolean;
      centreAt?: Dimensions;
    },
  ) {
    // Handle the options parameters.
    const {
      showName = true,
      skipStackChange = false,
      centreAt = null,
    } = options || {};
    const url = info.view;
    const transform = info.transform;
    const matrix = info.matrix;
    if (!isImageReady(img)) {
      LOG('Image not ready, re-schedule late loading draw of %s', url);
      showWaitingState(true);
      handleImageError(img);
      img.onload = function () {
        LOG('Late load of ', url);
        setImageReady(img, true);
        drawImage(state, img, info, options);
      };
      return;
    }
    if (!skipStackChange) {
      updateStackVisibility(info);
    }
    LOG_GROUP('Draw image');
    // Reset the canvas transform, clear it, and prepare to draw the (new) image.
    context!.setTransform(1, 0, 0, 1, 0, 0);
    context!.clearRect(0, 0, canvas.width, canvas.height);

    LOG('transform information: %o matrix information: %o', transform, matrix);
    LOG(
      'context %o, image %d×%d, natural %d×%d',
      state.canvasSize,
      img.width,
      img.height,
      img.naturalWidth,
      img.naturalHeight,
    );
    T_START('drawImage');
    const layout = layoutImage(
      state,
      new Dimensions(img.width, img.height),
      transform[0],
      centreAt,
      LOG,
    );
    context!.transform(
      matrix[0],
      matrix[1],
      matrix[2],
      matrix[3],
      layout.origin.x,
      layout.origin.y,
    );
    LOG(
      'Cory state: limits %o, offsets %o',
      cory.state.panLimits,
      cory.state.panOffsets,
    );
    context!.drawImage(
      img,
      layout.offsets.x,
      layout.offsets.y,
      layout.size.x,
      layout.size.y,
    );
    T_STOP('drawImage');
    LOG('exif:', info.exifhtml);
    imageInfoText!.innerHTML = info.exifhtml;
    showWaitingState(false);

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
    if (showName) {
      writeMessage(info.name);
    }
    LOG_GROUP_END();
  }

  function updateInfo(url: string) {
    LOG('Requesting info from ', url);
    getJSON<ViewInfo>(url, requestEnv).then(onInfoReceived, (err: unknown) => {
      // Without the image info, the viewer can't move on to other
      // images, so say so (and why), instead of looking busy.
      console.error('Failed to get the image info:', err);
      showWaitingState(false);
      showError(requestFailureMessage('get the image information', err));
    });
  }

  function redrawImage(centreAt?: Dimensions) {
    drawImage(cory.state, cory.state.img, cory.info.current, {
      showName: false,
      skipStackChange: true,
      centreAt: centreAt,
    });
    maybeWriteIsMovie(cory.info.current);
  }

  function incZoom() {
    adjustZoom(ZOOM_FACTOR, cory.state.pointerLocation);
  }

  function decZoom() {
    adjustZoom(1 / ZOOM_FACTOR, cory.state.pointerLocation);
  }

  // Adjust zoom by a factor.
  function adjustZoom(scaleRelative: number, centre?: Dimensions) {
    setZoom(cory.state.scale * scaleRelative, centre);
  }

  // Set zoom to a specific value.
  function setZoom(requested: number, centre?: Dimensions) {
    // If no-op, just return.
    if (requested == cory.state.scale) {
      return;
    }
    const { scale, message } = clampZoom(requested, cory.state.scale);
    if (message != null) {
      writeMessage(message, 1000);
    }
    // After clamping the scale to the allowed range, check again that we
    // do actually change the scale.
    if (scale == cory.state.scale) {
      return;
    }
    cory.state.scale = scale;
    // If we're not at full res, we need to load the full res image first.
    // This will call drawImage() when done.
    if (cory.state.img.dataset.fullres == 'false') {
      requestFullResImage(undefined, centre);
    } else {
      redrawImage(centre);
    }
  }

  // Set zoom to a specific value of pixel ratio. E.g. 1 is 1:1 pixel mapping, 2 is 200%, etc.
  function setPixelZoomRatio(pixelRatio: number, centreAt?: Dimensions) {
    if (cory.state.img.dataset.fullres == 'false') {
      LOG('load later!');
      requestFullResImage(
        () => setPixelZoomRatio(pixelRatio, centreAt),
        cory.state.pointerLocation,
      );
    } else {
      const location = centreAt ?? cory.state.pointerLocation;
      LOG(
        'setting pixel ratio to %f, absolute zoom %f, center point at %o',
        pixelRatio,
        pixelRatio * cory.state.scale11,
        location,
      );
      setZoom(pixelRatio * cory.state.scale11, location);
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

  function requestFullResImage(callback?: () => void, centreAt?: Dimensions) {
    const img = cory.state.img;
    if (img == null || img.dataset.fullres == 'true') {
      return;
    }
    writeMessage('Requesting full screen image…', 2000);
    showWaitingState(true);
    setImageReady(img, false);
    handleImageError(img);
    img.onload = function () {
      LOG('got full size image, calling drawImage');
      setImageReady(img, true);
      const c = cory.info.current;
      drawImage(cory.state, img, c, { centreAt: centreAt });
      if (callback != null) {
        callback();
      }
    };
    LOG('Requestiong full size image from', cory.info.current.bytes);
    img.dataset.fullres = 'true';
    img.src = cory.info.current.bytes;
  }

  function resizeCanvas(): boolean {
    // Read the computed (display) dimensions...
    const rect = context!.canvas.getBoundingClientRect();
    const width = rect.width || null;
    const height = rect.height || null;
    if (width == null || height == null) {
      // Unlikely, but...
      LOG(
        'Resizing canvas, failed to compute w/h, got %o x %o, assuming 300 pixels.',
        width,
        height,
      );
    }
    const scale = win.devicePixelRatio;
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
    context!.canvas.width = scaledWidth;
    context!.canvas.height = scaledHeight;
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
    img.src = imageUrlScaled(url, cory.state.canvasSize);
  }

  function handleImageError(img: HTMLImageElement) {
    img.onerror = function () {
      LOG('Image loading failed!');
      showWaitingState(false);
      win.alert('Failed to load the image!');
    };
  }

  function showWaitingState(waiting: boolean) {
    if (waiting) {
      canvas.style.cursor = 'wait';
      waitingBar!.style.display = 'block';
    } else {
      canvas.style.cursor = 'auto';
      waitingBar!.style.display = 'none';
    }
  }

  function setImageReady(img: HTMLImageElement, done: boolean) {
    if (done) {
      readyImages.add(img);
    } else {
      readyImages.delete(img);
    }
  }

  function isImageReady(img: HTMLImageElement) {
    return readyImages.has(img);
  }

  function handleOffscreenImageLoad(img: HTMLImageElement, kind: string) {
    setImageReady(img, true);
    LOG('Loaded', kind, ' image');
    T_START('post-load');
    // Hack to force pre-rendering. Seems to work, at least in FF
    // and Chrome. For large images on a certain machine, goes
    // from 600ms to ~15 ms (FF), ~0.1ms (Chrome).
    offContext!.drawImage(img, 0, 0);
    T_STOP('post-load');
  }

  function requestOffscreenImage(
    img: HTMLImageElement,
    info: ImageInfo | undefined,
    text: string,
  ) {
    if (info != null) {
      // Since the image is off-screen and won't be drawn, we don't need
      // to explicitly handle errors, as those will be handled once the
      // image wants to become on-screen.
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
    cory.prev = new win.Image();
    requestOffscreenImage(cory.prev, cory.info.prev, 'prev');
    cory.next = new win.Image();
    requestOffscreenImage(cory.next, cory.info.next, 'next');
  }

  /**
   * Emulates full screen by hiding the navigation menu and expanding the
   * main area, for when the browser's full screen API is not available.
   */
  function enterFakeFullScreen() {
    LOG('entering fake full screen');
    cory.state.fullscreen = true;
    navMenu.classList.add('nav-hidden');
    divMain.classList.add('fake-fullscreen');
    resizeCanvasAndRedraw();
  }

  function leaveFakeFullScreen() {
    LOG('exiting fake full screen');
    cory.state.fullscreen = false;
    navMenu.classList.remove('nav-hidden');
    divMain.classList.remove('fake-fullscreen');
    resizeCanvasAndRedraw();
  }

  function enterFullScreen() {
    const div = divMain;
    if (screenfull.isEnabled) {
      LOG('entering full screen via screenfull');
      // The browser can still refuse (e.g. due to a permissions policy, or
      // when not triggered by a user action); fall back to emulating it.
      screenfull.request(div).catch((err: unknown) => {
        console.warn('Full screen refused, emulating it instead:', err);
        enterFakeFullScreen();
        updateFullScreenIcon();
      });
    } else {
      enterFakeFullScreen();
    }
  }

  function leaveFullScreen() {
    // Check the actual mode, as entering real full screen might have
    // fallen back to the fake one.
    if (screenfull.isEnabled && screenfull.isFullscreen) {
      LOG('exiting full screen via screenfull');
      screenfull.exit().catch((err: unknown) => {
        console.error('Failed to exit full screen:', err);
      });
    } else {
      leaveFakeFullScreen();
    }
  }

  /**
   * Updates the full screen icon based on the current fullscreen state.
   */
  function updateFullScreenIcon() {
    if (cory.state.fullscreen) {
      fullScreenIcon.classList.remove('fa-maximize');
      fullScreenIcon.classList.add('fa-minimize');
    } else {
      fullScreenIcon.classList.remove('fa-minimize');
      fullScreenIcon.classList.add('fa-maximize');
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

  // Switches to a non-preloaded image and schedules its drawing.
  function switchToImage(info: ImageInfo) {
    const image = new win.Image();
    handleImageError(image);
    image.onload = function () {
      setImageReady(image, true);
      drawImage(cory.state, image, info);
    };
    writeMessage(`Loading ${info.name}...`);
    showWaitingState(true);
    maybeWriteIsMovie(info);
    loadImage(image, info.bytes);
    updateInfo(info.info);
  }

  function clearMessageAndTimeout() {
    if (cory.state.msgTimeId > 0) {
      win.clearTimeout(cory.state.msgTimeId);
      cory.state.msgTimeId = 0;
    }
    msgBox.style.visibility = 'hidden';
  }

  function writeMessage(message: string, timeout?: number) {
    clearMessageAndTimeout();
    msgBox.textContent = message;
    msgBox.style.visibility = 'visible';
    if (timeout == null) {
      timeout = 2000;
    }
    if (timeout > 0) {
      cory.state.msgTimeId = win.setTimeout(function () {
        cory.state.msgTimeId = 0;
        clearMessageAndTimeout();
      }, timeout);
    }
  }
  /** Shows an error, until the next message replaces it. */
  function showError(message: string) {
    writeMessage(message, 0);
  }

  function writePersistent(message: string) {
    persistBox.textContent = message;
  }

  function maybeWriteIsMovie(info: ImageInfo) {
    writePersistent(movieMessage(info));
  }

  function updateNavbar(topinfo: ViewInfo) {
    const setLink = (n: number, href: string, text: string) => {
      document.getElementById(`navlink${n}`)?.setAttribute('href', href);
      const textElement = document.getElementById(`navtext${n}`);
      if (textElement != null) {
        textElement.textContent = text;
      }
    };
    setLink(1, topinfo.yearurl, topinfo.year);
    setLink(2, topinfo.folderurl, topinfo.folder);
    setLink(3, topinfo.imageurl, topinfo.image);
  }

  /**
   * Advances to the next image or, in movie mode, starts playing the movie.
   */
  function advanceOrPlay() {
    if (cory.info.current.movie != null) {
      /* In case the movie was already played, let's just navigate to the
       * next image, so that a single action can be used for both movies and
       * images. */
      const video = cory.state.video;
      LOG(video?.paused, video?.currentTime, video?.duration);
      if (video != null && video.paused && video.currentTime == video.duration)
        advanceImage(true);
      else launchMovie();
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
    drawImage(cory.state, img, info);
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
    showWaitingState(true);
    getJSON<ViewInfo>(bootData('random-url', document) ?? '', requestEnv).then(
      (json) => switchToImage(json.current),
      (err: unknown) => {
        showWaitingState(false);
        showError(requestFailureMessage('get a random image', err));
      },
    );
  }

  function gotoFolder() {
    navigate(cory.info.folderurl);
  }

  /** Change display state for elements maching a given class */
  function changeVisibility(className: string, visible: boolean) {
    document.querySelectorAll(className).forEach(function (v) {
      if (v instanceof win.HTMLElement) {
        v.style.display = visible ? 'block' : 'none';
      }
    });
  }

  /** Toggle item visibility */
  function toggleItemVisibility(item: HTMLElement) {
    if (item.style.display == 'block') {
      item.style.display = 'none';
    } else {
      item.style.display = 'block';
    }
  }

  /** Event lander for when the movie is ready to play.
   *
   * It hides the (picture) canvas, and shows the video element.
   */
  function movieFrameAvailable(evt: Event) {
    LOG('movie frame available');
    const video = evt.target as HTMLVideoElement;
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
        // play() fails if the browser blocks playback, or the movie can't
        // be loaded; in that case, revert the optimistic icon change.
        const video = cory.state.video;
        video.play().catch((err: unknown) => {
          if (cory.state.video !== video) {
            // We already navigated away; the button is no longer ours.
            return;
          }
          moviePlaySpan.classList.remove('fa-pause');
          moviePlaySpan.classList.add('fa-play');
          const msg = playFailureMessage(err);
          if (msg != null) {
            console.error('Failed to play movie:', err);
            writePersistent(msg);
          }
        });
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
      seekBar.disabled = true;
      changeVisibility('.nav-only-image', false);
      changeVisibility('.nav-only-video', true);
      seekBar.valueAsNumber = 0;
      video.addEventListener('timeupdate', function () {
        seekBar.valueAsNumber = seekPercent(video.currentTime, video.duration);
      });
    } else {
      LOG('switching to picture mode');
      changeVisibility('.nav-only-image', true);
      changeVisibility('.nav-only-video', false);
      dropCurrentVideo();
    }
  }

  function flagImage(flag: boolean) {
    sendFlag(cory.info.current.flag, flag, requestEnv).then(
      (json) => writeMessage(json.text, 2000),
      (err: unknown) => showError(flagErrorMessage(err)),
    );
  }

  function setupTouchAndGestureHandlers() {
    // Track pointer start position and time for gesture detection
    const pointerStart = new Dimensions(0, 0);
    let pointerStartTime = 0;
    let lastTapTime = 0;
    let pointerId = -1;

    // Add pinch-zoom tracking
    const activePointers = new Map<number, Dimensions>();
    let initialPinchDistance = 0;
    let prevCenter = new Dimensions(0, 0);
    LOG('X: setupTouchAndGestureHandlers()');

    // Handle pointer down
    canvas.addEventListener(
      'pointerdown',
      function (e) {
        LOG('X: pointerdown ' + e.pointerId);

        // Store this pointer
        activePointers.set(e.pointerId, new Dimensions(e.offsetX, e.offsetY));

        // If this is the first or only pointer, track for swipe/tap
        if (activePointers.size === 1) {
          pointerId = e.pointerId;
          pointerStart.x = e.offsetX;
          pointerStart.y = e.offsetY;
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
        if (activePointers.size === 0) {
          if (e.isPrimary) {
            cory.state.pointerLocation.x = e.offsetX;
            cory.state.pointerLocation.y = e.offsetY;
          }
          return;
        }
        // After this, there's at least one pointer down.
        const prevPointer = Array.from(activePointers.values())[0];
        const pointer = new Dimensions(e.offsetX, e.offsetY);
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
                adjustZoom(scaleFactor, center);

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
        const pointerEnd = new Dimensions(e.offsetX, e.offsetY);
        const delta = pointerEnd.minus(pointerStart);
        const action = classifyPointerUp(
          pointerDuration,
          delta,
          pointerEndTime - lastTapTime,
          cory.state.scale,
        );
        LOG(
          'X: pointerup %d duration %d movement %o: %s',
          e.pointerId,
          pointerDuration,
          delta,
          action,
        );
        switch (action) {
          case 'previous':
            // Right swipe
            advanceImage(false);
            break;
          case 'next':
            // Left swipe
            advanceImage(true);
            break;
          case 'double-tap':
            // Zoom at point.
            toggleZoomFit(pointerEnd);
            lastTapTime = 0; // Reset to prevent triple tap detection
            break;
          case 'tap':
            launchMovie();
            lastTapTime = pointerEndTime;
            break;
          default:
            // A vertical swipe, ignored, or a pan _end_; we should have
            // handled the actual pan during the pointermove events, so
            // nothing to do here.
            break;
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
    // behavior. This is needed as a gesture event on a trackpad generates
    // both a mousewheel, but in the wrong direction compared to normal
    // mouse wheel.
    for (const name of ['gesturestart', 'gesturechange', 'gestureend']) {
      document.addEventListener(
        name,
        function (e) {
          LOG(`X: ${name} prevented`);
          e.preventDefault();
        },
        { passive: false },
      );
    }

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

    canvas.addEventListener('wheel', (event) => {
      // We don't want generally any scrolling on the canvas, as the
      // sizing is now fixed to the viewport.
      event.preventDefault();
      LOG(
        `MOUSEWHEEL:${event.deltaMode} ${event.deltaX} ${event.deltaY} ${event.deltaZ}`,
      );
      const ratio = wheelZoomRatio(event.deltaX, event.deltaY);
      if (ratio != null) {
        adjustZoom(ratio, new Dimensions(event.offsetX, event.offsetY));
      }
    });
  }
  setupTouchAndGestureHandlers();
  /// Toggles the help div.
  function toggleHelp() {
    helpModal.toggle();
  }

  /// Toggles between fit and 100% pixel ratios.
  function toggleZoomFit(centreAt?: Dimensions) {
    if (cory.state.scale == 1) setPixelZoomRatio(1, centreAt);
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
      case 'L':
        triggerFolderListMode();
        break;
      case 'i':
        toggleItemVisibility(imageInfo);
        break;
      case 'o':
        triggerGoToImageInfo();
        break;
      case 'D':
        triggerImageDownload();
        break;
      case 'ArrowUp':
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
        history.back();
        break;
      case '>':
        // Allow this to navigate forwards. Useful in full-screen mode, to
        // provide parity with backwards.
        history.forward();
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
  win.addEventListener('popstate', (event) => {
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

  function triggerGoToImageInfo() {
    navigate(cory.info.imageurl);
  }
  function triggerImageDownload() {
    downloadFile(document, cory.info.current.bytes, cory.info.current.name);
  }

  function triggerBrowseMode() {
    navigate(cory.info.current.browse);
  }

  function triggerListMode() {
    navigate(cory.info.current.list);
  }

  function triggerFolderListMode() {
    navigate(cory.info.folderlist);
  }

  // Based on current layout, convert the div#main to fixed
  // position, keeping - hopefully - same absolute location inside
  // the window.
  function mainToFixed() {
    // Hide footer.
    const footer = document.querySelector('footer');
    if (footer != null) {
      footer.style.display = 'none';
    }
    // For viewer app, we want the divmain to be fixed-viewport, and not
    // grow, as it is for normal "text" pages.
    divMain.style.overflow = 'hidden';
    divMain.style.position = 'relative';
    // Also remove container-fluid, as here we want as much display space
    // as possible.
    divMain.classList.remove('container-fluid');
    onClick('imageFull', function () {
      toggleFullScreen();
    });
    onClick('imageRand', function () {
      gotoRandomImage();
    });
    onClick('goImageInfo', triggerGoToImageInfo);
    onClick('downloadImage', triggerImageDownload);
    onClick('goParent', gotoFolder);
    onClick('goBrowse', triggerBrowseMode);
    onClick('goList', triggerListMode);
    onClick('goFolderList', triggerFolderListMode);
    onClick('imageZoom', () => {
      // If the zoom button is clicked, we don't want to toggle around the
      // zoom button's location, so explicitly override to centre.
      toggleZoomFit(cory.convertToCss(new Dimensions(0, 0)));
    });
    onClick('imagePrev', function () {
      advanceImage(false);
    });
    onClick('imageNext', function () {
      advanceImage(true);
    });
    onClick('folderPrev', function () {
      advanceFolder(false);
    });
    onClick('folderNext', function () {
      advanceFolder(true);
    });

    // movie-specific controls
    onClick('moviePlay', function () {
      launchMovie();
    });

    seekBar.addEventListener('input', function () {
      if (cory.state.video != null) {
        cory.state.video.currentTime = seekTime(
          cory.state.video.duration,
          seekBar.valueAsNumber,
        );
      }
    });
  }

  mainToFixed();
  resizeCanvas();
  maybeWriteIsMovie(bootinfo.current);

  const image = new win.Image();
  handleImageError(image);
  image.onload = function () {
    setImageReady(image, true);
    const c = bootinfo.current;
    drawImage(cory.state, image, c);
  };
  showWaitingState(true);
  loadImage(image, bootinfo.current.bytes);

  // Process the rest of info (load prev/next images) only after the
  // current image loading has been triggered, for faster startup.
  onInfoReceived(bootinfo);

  if ('ResizeObserver' in win) {
    const resizeObserver = new win.ResizeObserver((entries) => {
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
    (win as Window).addEventListener('resize', resizeCanvasAndRedraw, false);
  }
  return cory;
}
