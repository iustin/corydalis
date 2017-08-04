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
    var bootdiv = $("#boot");
    var booturl = bootdiv.data("bytes-url");
    var infourl = bootdiv.data("info-url");
    var debug = bootdiv.data("debug");
    var LOG = debug ? console.log.bind(console) : function () {};
    var T_START = debug ? console.time.bind(console) : function () {};
    var T_STOP =  debug ? console.timeEnd.bind(console) : function () {};


    var cory = {
        info: null,
        state: {
            fullscreen: false,
            img: null,
            lastX: 0,
            msgTimeId: null
        }
    };

    var divMain = $('#main');
    var canvas = $('#imageCanvas')[0];
    var context = canvas.getContext('2d');
    var msgCanvas = $('#messageCanvas')[0];
    var msgCtx = msgCanvas.getContext('2d');

    // Virtual (not-in-DOM) canvas that is used to for pre-rendering
    // images. The alternative would be to use putImageData() instead,
    // and pre-render explicitly the images, tracking said rendering,
    // etc., but this seems much easier.
    var offCanvas = document.createElement('canvas');
    var offContext = offCanvas.getContext('2d');

    function drawImage(img, url) {
        if (!isImageReady(img)) {
            img.onload = function() {
                LOG("Late load of ", url);
                setImageState(img, true);
                drawImage(img, url);
            };
            return;
        }

        context.clearRect(0, 0, canvas.width, canvas.height);
        var cW = $(context.canvas).width();
        var cH = $(context.canvas).height();
        var scaleX = img.width / cW;
        var scaleY = img.height / cH;
        var scale = scaleX >= scaleY ? scaleX : scaleY;
        var targetW = img.width / scale;
        var targetH = img.height / scale;
        var offX = targetW < cW ? (cW - targetW) / 2 : 0;
        var offY = targetH < cH ? (cH - targetH) / 2 : 0;
        LOG("pre-draw; imgW: ", img.width, ", imgH: ", img.height,
            ", cW: ", cW, ", cH: ", cH,
            ", scaleX: ", scaleX, ", scaleY: ", scaleY);
        cory.state.lastX = offX;
        T_START("drawImage");
        context.drawImage(img, offX, offY, img.width / scale, img.height / scale);
        T_STOP("drawImage");
        LOG("post-draw ", url);
        LOG("url: ", url, "location: ", location.href);
        if (url != location.href && url != undefined) // Prevent double entries.
            history.pushState(null, null, url);
        cory.state.img = img;
        cory.state.url = url;
    };

    function updateInfo(url) {
        LOG("Requesting info from ", url);
        $.ajax({url: url,
                type: "GET",
                dataType: "json",
               }).done(onInfoReceived);
    };

    function redrawImage() {
        drawImage(cory.state.img, cory.state.url);
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

    function requestImage(img, info, text) {
        if (info) {
            img.onload = function() {
                handleImageLoad(img, text);
            };
            $(img).data("done", false);
            img.src = info.bytes;
        } else {
            LOG("skipping", text, "image as unavailable");
        }
    }

    function onInfoReceived(json) {
        LOG("got cory");
        updateNavbar(json);
        cory.info = json;
        cory.prev = new Image();
        requestImage(cory.prev, cory.info.prev, "prev");
        cory.next = new Image();
        requestImage(cory.next, cory.info.next, "next");
    };

    function enterFullScreen() {
        var div = divMain[0];
        if (div.mozRequestFullScreen)
            div.mozRequestFullScreen();
        if(div.webkitRequestFullScreen)
            div.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
        cory.state.fullscreen = true;
    };

    function leaveFullScreen() {
        if (document.mozCancelFullScreen)
            document.mozCancelFullScreen();
        if (document.webkitCancelFullScreen)
            document.webkitCancelFullScreen();
        cory.state.fullscreen = false;
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
            writeMessage(info.name);
            drawImage(image, info.view);
        };
        writeMessage("Loading " + info.name + "...");
        image.src = info.bytes;
        updateInfo(info.info);
    }

    function clearMessage() {
        if (cory.state.msgTimeId) {
            window.clearTimeout(cory.state.msgTimeId);
        }
        msgCtx.clearRect(0, 0, msgCanvas.width, msgCanvas.height);
    }

    function writeMessage(text) {
        clearMessage();
        msgCtx.shadowOffsetX = 2;
        msgCtx.shadowOffsetY = 2;
        msgCtx.shadowBlur = 2;
        msgCtx.shadowColor = 'rgba(255, 255, 255, 1)';

        msgCtx.fillStyle = 'Black';
        msgCtx.textBaseline = 'top';
        msgCtx.font = 'x-large Sans';

        textWidth = Math.ceil(msgCtx.measureText(text).width);
        msgCtx.fillText(text, 0, 0);
        cory.state.msgTimeId = window.setTimeout(function() {
            cory.state.msgTimeId = null;
            clearMessage();
        }, 2000);
    }

    function updateNavbar(topinfo) {
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
        drawImage(img, viewurl);
        writeMessage(info.name);
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

    function setupHammer() {
        var mc = new Hammer.Manager(canvas, {});
        mc.add( new Hammer.Swipe({direction: Hammer.DIRECTION_ALL}));
        mc.add( new Hammer.Tap({pointers: 2}));
        mc.on("swiperight", function(ev) {advanceImage(false);});
        mc.on("swipeleft", function(ev) {advanceImage(false);});
        mc.on("swipeup", function(ev) {toggleFullScreen();});
        mc.on("swipedown", function(ev) {gotoRandomImage();});
        mc.on("tap", function(ev) {toggleFullScreen();});
    }

    setupHammer();

    $(document).keydown(function(e){
        switch (e.keyCode) {
        case 70: // 'f'
            toggleFullScreen();
            break;
        case 82: // 'r'
            gotoRandomImage();
            break;
        case 37: // left arrow
            advanceImage(false);
            break;
        case 39: // right arrow
            advanceImage(true);
            break;
        case 36: // home key
            switchToImage(cory.info.first);
            break;
        case 35: // end key
            switchToImage(cory.info.last);
            break;
        }
    });

    function computeNavBarHeight() {
        var navbar = $("nav.navbar");
        return navbar.outerHeight();
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

    updateInfo(infourl);

    var image = new Image();
    image.onload = function() {
        setImageState(image, true);
        drawImage(image);
    };
    image.src = booturl;

    window.addEventListener('resize', resizeCanvasAndRedraw, false);
    window.addEventListener('orientationchange', resizeCanvasAndRedraw, false);

});
