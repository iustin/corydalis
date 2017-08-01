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


    var cory = {
        info: null,
        state: {
            fullscreen: false,
            img: null,
            lastX: 0
        }
    };

    var canvas = $('#imageCanvas')[0];
    var context = canvas.getContext('2d');

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
        context.drawImage(img, offX, offY, img.width / scale, img.height / scale);
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
        // Read the computed (display) dimensions...
        var width = $(context.canvas).width();
        var height = $(context.canvas).height();
        LOG("Resizing canvas, width ", width, ", height ", height);
        // to set the model (coordinate) dimension.
        context.canvas.width = width;
        context.canvas.height = height;
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

    function onInfoReceived(json) {
        LOG("got cory");
        cory.info = json;
        cory.prev = new Image();
        $(cory.prev).data("done", false);
        cory.prev.onload = function() {
            setImageState(cory.prev, true)
            LOG("Loaded prev image");
        };
        cory.prev.src = cory.info.prev.bytes;
        cory.next = new Image();
        $(cory.next).data("done", false);
        cory.next.onload = function() {
            setImageState(cory.next, true)
            LOG("Loaded next image");
        };
        cory.next.src = cory.info.next.bytes;
    };

    function enterFullScreen() {
        if (canvas.mozRequestFullScreen)
            canvas.mozRequestFullScreen();
        if(canvas.webkitRequestFullScreen)
            canvas.webkitRequestFullScreen(Element.ALLOW_KEYBOARD_INPUT);
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

    function writeMessage(text) {
        context.save();
        context.shadowOffsetX = 4;
        context.shadowOffsetY = 4;
        context.shadowBlur = 2;
        context.shadowColor = 'rgba(255, 255, 255, 0.7)';

        context.font = '20px Sans';
        context.fillStyle = 'Black';
        context.fillText(text, cory.state.lastX + 10, 30);
        context.restore();

        window.setTimeout(redrawImage, 3000);
    }

    $(document).keydown(function(e){
        switch (e.keyCode) {
        case 70: // 'f'
            toggleFullScreen();
            break;
        case 37: // left arrow
            drawImage(cory.prev, cory.info.prev.view);
            writeMessage(cory.info.prev.view);
            updateInfo(cory.info.prev.info);
            break;
        case 39: // right arrow
            drawImage(cory.next, cory.info.next.view);
            updateInfo(cory.info.next.info);
            break;
        }
    });

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
