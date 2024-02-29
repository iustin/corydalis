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
/// <reference types="hammerjs"/>
/// <reference types="screenfull"/>

// The Haskell types.

type Transform = [number, boolean, boolean];

type AffineMatrix = [number, number, number, number];

type Url = string

type ImageInfo = {
  info: Url,
  bytes: Url,
  movie?: Url,
  view: Url,
  flag: Url,
  name: string,
  transform: Transform,
  matrix: AffineMatrix,
}

type ViewInfo = {
  year: string,
  yearurl: Url,
  folder: string,
  folderurl: Url,
  image: string,
  imageurl: Url,
  first: ImageInfo,
  prevfolder?: ImageInfo,
  prev?: ImageInfo,
  current: ImageInfo,
  next?: ImageInfo,
  nextfolder?: ImageInfo,
  last: ImageInfo,
}

// Internal state

type State = {
  fullscreen: boolean,
  img: HTMLImageElement,
  lastX: number,
  msgTimeId: number,
  transform: Transform,
  matrix: AffineMatrix,
  url: string,
}

type Cory = {
  info: ViewInfo,
  prev: HTMLImageElement,
  next: HTMLImageElement,
  state: State,
}


$(function () {
  const bootdiv = $('#boot');
  const bootinfo = bootdiv.data('view-info');
  const debug = bootdiv.data('debug');
  const LOG = debug ? console.log.bind(console) : function () { };
  const T_START = debug ? console.time.bind(console) : function () { };
  const T_STOP = debug ? console.timeEnd.bind(console) : function () { };

  LOG('bootinfo ', bootinfo);

  const cory: Cory = {
    info: bootinfo,
    prev: new Image(),
    next: new Image(),
    state: {
      fullscreen: false,
      img: new Image(),
      lastX: 0,
      msgTimeId: 0,
      transform: bootinfo.current.transform,
      matrix: bootinfo.current.matrix,
      url: location.href,
    },
  };

  const divMain = $('#main');
  const navMenu = $('#nav');

  const canvas = <HTMLCanvasElement>$('#imageCanvas')[0];
  const context = canvas.getContext('2d');
  const msgBox = $('#messageBox');
  const persistBox = $('#persistBox');

  // Virtual (not-in-DOM) canvas that is used to for pre-rendering
  // images. The alternative would be to use putImageData() instead,
  // and pre-render explicitly the images, tracking said rendering,
  // etc., but this seems much easier.
  const offCanvas = document.createElement('canvas');
  const offContext = offCanvas.getContext('2d');

  if (canvas == null ||
    context == null ||
    offCanvas == null ||
    offContext == null) {
    LOG('Initialising canvas elements failed!');
    window.alert('Cannot fully initialise the application, aborting!');
    return;
  }

  // Draws an already-loaded image into a give image element.
  function drawImage(img: HTMLImageElement, url: string, transform: Transform,
                     matrix: AffineMatrix, msg?: string) {
    if (!isImageReady(img)) {
      img.onload = function () {
        LOG('Late load of ', url);
        setImageState(img, true);
        drawImage(img, url, transform, matrix, msg);
      };
      return;
    }
    if (context == null) {
      return;
    }

    context.setTransform(1, 0, 0, 1, 0, 0);
    context.clearRect(0, 0, canvas.width, canvas.height);
    const deviceScale = window.devicePixelRatio;
    let cW = $(context.canvas).width();
    if (cW == null) {
      // Unlikely, as we pass a DOM object, but TS...
      cW = 300;
    }
    cW = Math.floor(cW * deviceScale);
    let cH = $(context.canvas).height();
    if (cH == null) {
      // Unlikely, as we pass a DOM object, but TS...
      cH = 300;
    }
    cH = Math.floor(cH * deviceScale);
    LOG('transform information:', transform, 'matrix information:', matrix);
    const rotation = transform[0];
    const imgW = rotation == 0 ? img.width : img.height;
    const imgH = rotation == 0 ? img.height : img.width;
    const scaleX = imgW / cW;
    const scaleY = imgH / cH;
    const scale = scaleX >= scaleY ? scaleX : scaleY;
    // Note: target* must be in original coordinate system, not
    // rotated! So using img.width, not imgW. This is because from
    // the point of view of the image, it's drawn straight, not
    // rotated. Sigh, head hurts.
    const targetW = Math.round(img.width / scale);
    const targetH = Math.round(img.height / scale);
    let offX = targetW < cW ? Math.round((cW - targetW) / 2) : 0;
    let offY = targetH < cH ? Math.round((cH - targetH) / 2) : 0;
    LOG('pre-draw; imgW:', imgW, 'imgH:', imgH,
      'cW:', cW, 'cH:', cH,
      'scaleX:', scaleX, 'scaleY:', scaleY,
      'targetW:', targetW, 'targetH', targetH);
    cory.state.lastX = offX;
    T_START('drawImage');
    LOG('transform call:', matrix, cW / 2, cH / 2);
    context.transform(matrix[0], matrix[1], matrix[2], matrix[3], cW / 2, cH / 2);
    offX -= cW / 2;
    offY -= cH / 2;
    LOG('draw call:', offX, offY, targetW, targetH);
    context.drawImage(img, offX, offY, targetW, targetH);
    T_STOP('drawImage');
    LOG('post-draw ', url);
    LOG('url: ', url, 'location: ', location.href);
    // Prevent double entries.
    if (url != location.href && url != undefined) {
      history.pushState(null, '', url);
    }
    cory.state.img = img;
    cory.state.url = url;
    cory.state.transform = transform;
    cory.state.matrix = matrix;
    if (msg != null) {
      writeMessage(msg);
    }
  };

  function updateInfo(url: string) {
    LOG('Requesting info from ', url);
    $.ajax({
      url: url,
      type: 'GET',
      dataType: 'json',
    }).done(onInfoReceived);
  };

  function redrawImage() {
    drawImage(cory.state.img, cory.state.url,
              cory.state.transform, cory.state.matrix);
    maybeWriteIsMovie(cory.info.current);
  };

  function resizeCanvas() {
    if (context == null) {
      return;
    }
    // Reset main div top position, in case navbar changed size.
    divMain.css({ 'top': computeNavBarHeight() });
    // Read the computed (display) dimensions...
    const width = $(context.canvas).width();
    const height = $(context.canvas).height();
    if (width == null || height == null) {
      // Unlikely, but...
      LOG('Resizing canvas, failed to compute w/h, width: ',
        width, ', height: ', height, ', aborting.');
      return;
    }
    const scale = window.devicePixelRatio;
    const scaledWidth = Math.floor(width * scale);
    const scaledHeight = Math.floor(height * scale);
    LOG('Resizing canvas, width ', width, ', height ', height,
      'scaled: ', scaledWidth, ' x ', scaledHeight);
    // to set the model (coordinate) dimension.
    context.canvas.width = scaledWidth;
    context.canvas.height = scaledHeight;
  };

  function resizeCanvasAndRedraw() {
    resizeCanvas();
    redrawImage();
  };

  function setImageState(img: HTMLImageElement, done: boolean) {
    $(img).data('done', done);
  }

  function isImageReady(img: HTMLImageElement) {
    return $(img).data('done');
  }

  function handleOffscreenImageLoad(img: HTMLImageElement, kind: string) {
    setImageState(img, true);
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
    let w = $(canvas).width();
    if (w == null) {
      w = 300;
    }
    let h = $(canvas).height();
    if (h == null) {
      h = 300;
    }
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

  function requestOffscreenImage(img: HTMLImageElement,
    info: ImageInfo | undefined, text: string) {
    if (info != null) {
      img.onload = function () {
        handleOffscreenImageLoad(img, text);
      };
      $(img).data('done', false);
      img.src = imageUrlScaled(info.bytes);
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
  };

  function enterFullScreen() {
    cory.state.fullscreen = true;
    const div = divMain[0];
    if (screenfull.isEnabled) {
      LOG('entering full screen via screenfull');
      // Sigh, why is the full type not seen correctly?
      (screenfull as screenfull.Screenfull).request(div);
    } else {
      LOG('entering fake full screen');
      navMenu.addClass('nav-hidden');
      divMain.addClass('fake-fullscreen');
      resizeCanvasAndRedraw();
    }
  };

  function leaveFullScreen() {
    cory.state.fullscreen = false;
    if (screenfull.isEnabled) {
      LOG('exiting full screen via screenfull');
      (screenfull as screenfull.Screenfull).exit();
    } else {
      LOG('exiting fake full screen');
      navMenu.removeClass('nav-hidden');
      divMain.removeClass('fake-fullscreen');
      resizeCanvasAndRedraw();
    }
  }

  function toggleFullScreen() {
    if (cory.state.fullscreen) {
      leaveFullScreen();
    } else {
      enterFullScreen();
    }
  }

  // Switches to a non-preloaded image.
  function switchToImage(info: ImageInfo) {
    const image = new Image();
    image.onload = function () {
      setImageState(image, true);
      drawImage(image, info.view, info.transform, info.matrix, info.name);
    };
    writeMessage(`Loading ${info.name}...`);
    maybeWriteIsMovie(info);
    image.src = imageUrlScaled(info.bytes);
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

  function maybeWriteIsMovie(info: { movie?: Url }) {
    writePersistent(
      info.movie != null ?
        'This is a movie. Press \'p\', click or touch to play.' :
        '');
  }

  function updateNavbar(topinfo: ViewInfo) {
    $('#navlink1').attr('href', topinfo.yearurl);
    $('#navtext1').text(topinfo.year);
    $('#navlink2').attr('href', topinfo.folderurl);
    $('#navtext2').text(topinfo.folder);
    $('#navlink3').attr('href', topinfo.imageurl);
    $('#navtext3').text(topinfo.image);
  }

  function advanceImage(forward: boolean) {
    const img = forward ? cory.next : cory.prev;
    const info = forward ? cory.info.next : cory.info.prev;
    if (!info) {
      writeMessage('No ' + (forward ? 'next' : 'previous') + ' image');
      return;
    }
    const viewurl = info.view;
    writeMessage('Loading ' + info.name, 6000);
    maybeWriteIsMovie(info);
    drawImage(img, viewurl, info.transform, info.matrix, info.name);
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

  function launchMovie() {
    if (cory.info.current.movie != null) {
      LOG('Opening in separate window:', cory.info.current.movie);
      window.open(cory.info.current.movie, '');
    }
  }

  function flagImage(flag: boolean) {
    $.ajax({
      url: cory.info.current.flag,
      type: flag ? 'PUT' : 'DELETE',
      dataType: 'json',
    }).done(function (json) {
      writeMessage(json.text, 2000);
    }).fail(function (xhr, status, details) {
      writeMessage('Error flagging image: ' + status + ', details: ' + details);
    });
  }

  function setupHammer() {
    const mc = new Hammer.Manager(canvas, {});
    // mc.add( new Hammer.Swipe({direction: Hammer.DIRECTION_HORIZONTAL}));
    mc.add(new Hammer.Pan({ direction: Hammer.DIRECTION_HORIZONTAL }));
    const singleTap = new Hammer.Tap({ event: 'singletap' });
    mc.add(singleTap);
    // mc.on("swiperight", function(ev) {advanceImage(false);});
    // mc.on("swipeleft", function(ev) {advanceImage(true);});
    mc.on('panend', function (ev) {
      LOG('end pan, direction ', ev.direction);
      switch (ev.direction) {
        case Hammer.DIRECTION_LEFT:
          advanceImage(true);
          break;
        case Hammer.DIRECTION_RIGHT:
          advanceImage(false);
          break;
        default: return;
      }
    });
    // mc.on("swipedown", function(ev) {toggleFullScreen();});
    // mc.on("swipeup", function(ev) {gotoRandomImage();});
    mc.on('doubletap', function (ev) {
      toggleFullScreen();
    });
    mc.on('singletap', function (ev) {
      launchMovie();
    });
  }

  setupHammer();

  $(document).on("keydown", function (e) {
    if (e.altKey || e.ctrlKey) {
      return;
    }
    const active = document.activeElement;
    if (active != null && active.id === 'entry') {
      return;
    }
    let handled = true;
    switch (e.keyCode) {
      case 40: // up arrow
      case 70: // 'f'
        toggleFullScreen();
        break;
      case 80: // 'p'
        launchMovie();
        break;
      case 82: // 'r'
        gotoRandomImage();
        break;
      case 85: // 'u'
        gotoFolder();
        break;
      case 88: // 'x'
        flagImage(true);
        break;
      case 78: // 'n'
        flagImage(false);
        break;
      case 37: // left arrow
        advanceImage(false);
        break;
      case 32: // space
      case 39: // right arrow
        advanceImage(true);
        break;
      case 33: // pg up
        advanceFolder(false);
        break;
      case 34: // pg down
        advanceFolder(true);
        break;
      case 36: // home key
        switchToImage(cory.info.first);
        break;
      case 35: // end key
        switchToImage(cory.info.last);
        break;
      default:
        handled = false;
        break;
    }
    if (handled) {
      e.preventDefault();
    }
  });

  function computeNavBarHeight(): number {
    if (cory.state.fullscreen) {
      return 0;
    } else {
      const navbar = $('nav.navbar');
      let h = navbar.outerHeight();
      if (h == null) {
        h = 0;
      }
      return h;
    }
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
      'top': navBarH,
      'position': 'fixed',
      'bottom': 0,
      'left': 0,
      'right': 0,
    });
    $('#imageFull').on("click", function (ev) {
      toggleFullScreen();
    });
    $('#imageRand').on("click", function (ev) {
      gotoRandomImage();
    });
    $('#imageUp').on("click", function (ev) {
      window.location.href = cory.info.folderurl;
    });
    $('#imagePrev').on("click", function (ev) {
      advanceImage(false);
    });
    $('#imageNext').on("click", function (ev) {
      advanceImage(true);
    });
    $('#folderPrev').on("click", function (ev) {
      advanceFolder(false);
    });
    $('#folderNext').on("click", function (ev) {
      advanceFolder(true);
    });
  }

  mainToFixed();
  resizeCanvas();
  maybeWriteIsMovie(bootinfo.current);

  const image = new Image();
  image.onload = function () {
    setImageState(image, true);
    const c = bootinfo.current;
    drawImage(image, location.href, c.transform, c.matrix, c.name);
  };
  image.src = imageUrlScaled(bootinfo.current.bytes);

  // Process the rest of info (load prev/next images) only after the
  // current image loading has been triggered, for faster startup.
  onInfoReceived(bootinfo);

  window.addEventListener('resize', resizeCanvasAndRedraw, false);
  window.addEventListener('orientationchange', resizeCanvasAndRedraw, false);
});
