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

$(document).ready(function() {
    'use strict';
    var bootdiv = $("#boot");
    var booturl = bootdiv.data("bytes-url");
    var bootflagurl = bootdiv.data("flag-url");
    var infourl = bootdiv.data("info-url");
    var boottrans = bootdiv.data("initial-transform");
    var bootmatrix = bootdiv.data("initial-matrix");
    var debug = bootdiv.data("debug");
    var LOG = debug ? console.log.bind(console) : function () {};
    var T_START = debug ? console.time.bind(console) : function () {};
    var T_STOP =  debug ? console.timeEnd.bind(console) : function () {};

    LOG("booturl", booturl, "infourl", infourl, "boottrans", boottrans);

    var cory = {
        info: null,
        state: {
            fullscreen: false,
            img: null,
            lastX: 0,
            msgTimeId: null,
            info: null,
            movie: null
        }
    };

    // Used in the initial image load to display the "is movie
    // message".
    var bootfakeinfo = {
        movie: bootdiv.data("movie"),
        flag: bootdiv.data("flag-url"),
    };
    LOG("boot fake info", bootfakeinfo);

    var divMain = $('#main');
    var navMenu = $('#nav');

    var canvas = $('#imageCanvas')[0];
    var context = canvas.getContext('2d');
    var msgCanvas = $('#messageCanvas')[0];
    var msgCtx = msgCanvas.getContext('2d');
    var persistCanvas = $('#persistCanvas')[0];
    var persistCtx = persistCanvas.getContext('2d');

    // Virtual (not-in-DOM) canvas that is used to for pre-rendering
    // images. The alternative would be to use putImageData() instead,
    // and pre-render explicitly the images, tracking said rendering,
    // etc., but this seems much easier.
    var offCanvas = document.createElement('canvas');
    var offContext = offCanvas.getContext('2d');

    function drawImage(img, url, transform, matrix, msg) {
        if (!isImageReady(img)) {
            img.onload = function() {
                LOG("Late load of ", url);
                setImageState(img, true);
                drawImage(img, url, transform, matrix, msg);
            };
            return;
        }

        context.setTransform(1, 0, 0, 1, 0, 0);
        context.clearRect(0, 0, canvas.width, canvas.height);
        var cW = $(context.canvas).width();
        var cH = $(context.canvas).height();
        LOG("transform information:", transform, "matrix information:", matrix);
        var rotation = transform[0];
        var imgW = rotation == 0 ? img.width : img.height;
        var imgH = rotation == 0 ? img.height : img.width;
        var scaleX = imgW / cW;
        var scaleY = imgH / cH;
        var scale = scaleX >= scaleY ? scaleX : scaleY;
        // Note: target* must be in original coordinate system, not
        // rotated! So using img.width, not imgW. This is because from
        // the point of view of the image, it's drawn straight, not
        // rotated. Sigh, head hurts.
        var targetW = Math.round(img.width / scale);
        var targetH = Math.round(img.height / scale);
        var offX = targetW < cW ? Math.round((cW - targetW) / 2) : 0;
        var offY = targetH < cH ? Math.round((cH - targetH) / 2) : 0;
        LOG("pre-draw; imgW:", imgW, "imgH:", imgH,
            "cW:", cW, "cH:", cH,
            "scaleX:", scaleX, "scaleY:", scaleY,
            "targetW:", targetW, "targetH", targetH);
        cory.state.lastX = offX;
        T_START("drawImage");
        LOG("transform call:", matrix, cW/2, cH/2);
        context.transform(matrix[0], matrix[1], matrix[2], matrix[3], cW/2, cH/2);
        offX -= cW/2;
        offY -= cH/2;
        LOG("draw call:", offX, offY, targetW, targetH);
        context.drawImage(img, offX, offY, targetW, targetH);
        T_STOP("drawImage");
        LOG("post-draw ", url);
        LOG("url: ", url, "location: ", location.href);
        if (url != location.href && url != undefined) // Prevent double entries.
            history.pushState(null, null, url);
        cory.state.img = img;
        cory.state.url = url;
        cory.state.transform = transform;
        cory.state.matrix = matrix;
        if (msg != null) {
            writeMessage(msg);
        }
    };

    function updateInfo(url) {
        LOG("Requesting info from ", url);
        $.ajax({url: url,
                type: "GET",
                dataType: "json",
               }).done(onInfoReceived);
    };

    function redrawImage() {
        drawImage(cory.state.img, cory.state.url, cory.state.transform, cory.state.matrix);
        maybeWriteIsMovie(cory.state);
    };

    function resizeCanvas() {
        // Reset main div top position, in case navbar changed size.
        divMain.css({"top": computeNavBarHeight()});
        // Read the computed (display) dimensions...
        var width = $(context.canvas).width();
        var height = $(context.canvas).height();
        LOG("Resizing canvas, width ", width, ", height ", height);
        // to set the model (coordinate) dimension.
        context.canvas.width = width;
        context.canvas.height = height;
        msgCtx.canvas.width = $(msgCtx.canvas).width();
        msgCtx.canvas.height = $(msgCtx.canvas).height();
        persistCtx.canvas.width = $(persistCtx.canvas).width();
        persistCtx.canvas.height = $(persistCtx.canvas).height();
    };

    function resizeCanvasAndRedraw() {
        resizeCanvas();
        redrawImage();
    };

    function setImageState(img, done) {
        $(img).data("done", done);
    }

    function isImageReady(img) {
        return $(img).data("done");
    }

    function handleImageLoad(img, kind) {
        setImageState(img, true);
        LOG("Loaded", kind, " image");
        T_START("post-load");
        // Hack to force pre-rendering. Seems to work, at least in FF
        // and Chrome. For large images on a certain machine, goes
        // from 600ms to ~15 ms (FF), ~0.1ms (Chrome).
        offContext.drawImage(img, 0, 0);
        T_STOP("post-load");
    }

    function imageUrlScaled(baseUrl) {
        var w = $(canvas).width();
        var h = $(canvas).height();
        var r = w > h ? w : h;
        // TODO: suply and read rendered sizes in/from boot data, and
        // make calls only for the right image sizes.
        var url = new URL(baseUrl);
        url.searchParams.set('res', r);
        return url.toString();
    }

    function requestImage(img, info, text) {
        if (info) {
            img.onload = function() {
                handleImageLoad(img, text);
            };
            $(img).data("done", false);
            var w = $(canvas).width();
            var h = $(canvas).height();
            img.src = imageUrlScaled(info.bytes);
        } else {
            LOG("skipping", text, "image as unavailable");
        }
    }

    function onInfoReceived(json) {
        LOG("got cory");
        updateNavbar(json);
        cory.info = json;
        cory.state.movie = cory.info.current.movie;
        cory.prev = new Image();
        requestImage(cory.prev, cory.info.prev, "prev");
        cory.next = new Image();
        requestImage(cory.next, cory.info.next, "next");
    };

    function enterFullScreen() {
        cory.state.fullscreen = true;
        var div = divMain[0];
        if (screenfull.enabled) {
            screenfull.request(div);
        } else {
            LOG("entering fake full screen");
            navMenu.addClass("nav-hidden");
            divMain.addClass("fake-fullscreen");
            resizeCanvasAndRedraw();
        }
    };

    function leaveFullScreen() {
        cory.state.fullscreen = false;
        if (screenfull.enabled) {
            screenfull.exit();
        } else {
            LOG("exiting fake full screen");
            navMenu.removeClass("nav-hidden");
            divMain.removeClass("fake-fullscreen");
            resizeCanvasAndRedraw();
        }
    }

    function toggleFullScreen() {
        if (cory.state.fullscreen)
            leaveFullScreen();
        else
            enterFullScreen();
    }

    // Switches to a non-preloaded image.
    function switchToImage(info) {
        var image = new Image();
        image.onload = function() {
            setImageState(image, true);
            drawImage(image, info.view, info.transform, info.matrix, info.name);
        };
        writeMessage("Loading " + info.name + "...");
        maybeWriteIsMovie(info);
        image.src = info.bytes;
        updateInfo(info.info);
    }

    function clearMessageAndTimeout() {
        if (cory.state.msgTimeId) {
            window.clearTimeout(cory.state.msgTimeId);
        }
        clearCanvasContext(msgCanvas, msgCtx);
    }

    function clearCanvasContext(canv, ctx) {
        ctx.clearRect(0, 0, canv.width, canv.height);
    }

    // Prepares a canvas context for writing text to it.
    function writeText(text, canv, ctx, font) {
        ctx.shadowOffsetX = 2;
        ctx.shadowOffsetY = 2;
        ctx.shadowBlur = 2;
        ctx.shadowColor = 'rgba(255, 255, 255, 1)';

        ctx.fillStyle = 'Black';
        ctx.textBaseline = 'top';
        ctx.font = font;

        ctx.fillText(text, 0, 0);
    }

    function writeMessage(text, timeout) {
        clearMessageAndTimeout();
        writeText(text, msgCanvas, msgCtx, 'x-large Sans');

        if (typeof timeout === 'undefined') {
            timeout = 2000;
        }
        if (timeout > 0) {
            cory.state.msgTimeId = window.setTimeout(function() {
                cory.state.msgTimeId = null;
                clearMessageAndTimeout();
            }, timeout);
        }
    }
    function writePersistent(text) {
        clearCanvasContext(persistCanvas, persistCtx);
        writeText(text, persistCanvas, persistCtx, 'italic large Sans');
    }

    function maybeWriteIsMovie(info) {
        clearCanvasContext(persistCanvas, persistCtx);
        if(info.movie) {
            writePersistent("This is a movie. Press 'p', click or touch to play.");
        }
    }

    function updateNavbar(topinfo) {
        $("#navlink1").attr("href", topinfo.yearurl);
        $("#navtext1").text(topinfo.year);
        $("#navlink2").attr("href", topinfo.folderurl);
        $("#navtext2").text(topinfo.folder);
        $("#navlink3").attr("href", topinfo.imageurl);
        $("#navtext3").text(topinfo.image);
    }

    function advanceImage(forward) {
        var img = forward ? cory.next : cory.prev;
        var info = forward ? cory.info.next : cory.info.prev;
        if (!info) {
            writeMessage("No " + (forward ? "next" : "previous") + " image");
            return;
        }
        var viewurl = info.view;
        writeMessage("Loading " + info.name, 6000);
        maybeWriteIsMovie(info);
        drawImage(img, viewurl, info.transform, info.matrix, info.name);
        updateInfo(info.info);
    }

    function gotoRandomImage() {
        $.ajax({url: bootdiv.data("random-url"),
                type: "GET",
                dataType: "json",
               }).done(function(json) {
                   switchToImage(json.current);
               });
    }

    function gotoFolder() {
        location.href = cory.info.folderurl;
    }

    function launchMovie() {
        if (cory.state.movie != null) {
            LOG("Opening in separate window:", cory.state.movie);
            window.open(cory.state.movie, "");
        }
    }

    function flagImage(flag) {
        $.ajax({url: cory.info.current.flag,
                type: flag ? "PUT" : "DELETE",
                dataType: "json",
               }).done(function(json) {
                   writeMessage(json.text, 2000);
               }).fail(function(xhr, status, details) {
                   writeMessage("Error flagging image: " + status + ", details: " + details);
               });
    }

    function setupHammer() {
        var mc = new Hammer.Manager(canvas, {});
        mc.add( new Hammer.Swipe({direction: Hammer.DIRECTION_HORIZONTAL}));
        var singleTap = new Hammer.Tap({event: "singletap"});
        var doubleTap = new Hammer.Tap({event: "doubletap", pointers: 2});
        mc.add([singleTap, doubleTap]);
        doubleTap.recognizeWith(singleTap);
        singleTap.requireFailure(doubleTap);
        mc.on("swiperight", function(ev) {advanceImage(false);});
        mc.on("swipeleft", function(ev) {advanceImage(true);});
        //mc.on("swipedown", function(ev) {toggleFullScreen();});
        //mc.on("swipeup", function(ev) {gotoRandomImage();});
        mc.on("doubletap", function(ev) {toggleFullScreen();});
        mc.on("singletap", function(ev) {launchMovie();});
    }

    setupHammer();

    $(document).keydown(function(e){
        if (e.altKey || e.ctrlKey) {
            return;
        }
        var handled = true;
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

    function computeNavBarHeight() {
        if (cory.state.fullscreen) {
            return 0;
        } else {
            var navbar = $("nav.navbar");
            return navbar.outerHeight();
        }
    }

    // Based on current layout, convert the div#main to fixed
    // position, keeping - hopefully - same absolute location inside
    // the window.
    function mainToFixed() {
        // Hide footer.
        $("footer").css("display", "none");
        // Remove container-fluid, as here we want as much display
        // space as possible.
        divMain.removeClass("container-fluid");
        // Compute current location (on screen), based on navbar
        // height.
        var navBarH = computeNavBarHeight();
        LOG("navbar at ", navBarH);
        // And convert to absolute at same location.
        divMain.css({
            "top": navBarH,
            "position": "fixed",
            "bottom": 0,
            "left": 0,
            "right": 0
        });
    }

    mainToFixed();
    resizeCanvas();
    maybeWriteIsMovie(bootfakeinfo);

    updateInfo(infourl);

    var image = new Image();
    image.onload = function() {
        setImageState(image, true);
        drawImage(image, location.href, boottrans, bootmatrix);
    };
    image.src = imageUrlScaled(booturl);

    window.addEventListener('resize', resizeCanvasAndRedraw, false);
    window.addEventListener('orientationchange', resizeCanvasAndRedraw, false);

});
