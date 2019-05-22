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
    $.fancybox.defaults.animationEffect = "false";
    $.fancybox.defaults.transitionEffect = "false";
    // Disable hashing since masonry controls the URL.
    $.fancybox.defaults.hash = false;

    // Custom after show to scroll the background item into view, in
    // order to trigger infinite scroll to load more images.
    $.fancybox.defaults.afterShow = function(instance, current) {
        if (!current) {
            return;
        }
        // FIXME: new elements being loaded doesn't actually trigger
        // updating the current gallery, need to fix. Stack overflow
        // question at
        // <https://stackoverflow.com/questions/49532246/next-image-doesnt-work-when-page-content-updated-by-infinite-scroll>
        // has some information but doesn't seem up-to-date.
        current.opts.$orig.get(0).scrollIntoView();
    }

    // Custom view item button.
    $.fancybox.defaults.btnTpl.view =
        '<button data-fancybox-view class="fancybox-button fancybox-button--view" title="Open image in viewer">' +
        '<span class="fas fa-external-link-alt">' +
        '</button>';

    $('body').on('click', '[data-fancybox-view]', function() {
        var instance = $.fancybox.getInstance(),
            current = instance.current || null;
        if (!current) {
            return;
        }
        if (!current.opts.viewurl) {
            $.fancybox.open('<div class="message"><h2>Internal Error!</h2>' +
                            '<p>Viewed image does not have a <i>viewurl</i> data attribute!</p></div>');
            console.log("No viewurl, returning", current);
            return;
        }

        window.open(current.opts.viewurl, '');
    });

    // Custom view item info button.
    $.fancybox.defaults.btnTpl.info =
        '<button data-fancybox-info class="fancybox-button fancybox-button--info" title="View image info">' +
        '<span class="fas fa-info-circle">' +
        '</button>';

    $('body').on('click', '[data-fancybox-info]', function() {
        var instance = $.fancybox.getInstance(),
            current = instance.current || null;
        if (!current) {
            console.log("No current, returning");
            return;
        }
        if (!current.opts.infourl) {
            $.fancybox.open('<div class="message"><h2>Internal Error!</h2>' +
                            '<p>Viewed image does not have a <i>infourl</i> data attribute!</p></div>');
            console.log("No infourl, returning", current);
            return;
        }
        window.open(current.opts.infourl, '');
    });

    $.fancybox.defaults.buttons = [
        'view',
        'info',
        'zoom',
        'close',
    ];

    // Trigger fancybox.
    $().fancybox({
        selector: 'div#fbox-container a.fbox-item',
        // TODO: to allow preload=false, need to pass height/width.
        preload: true,
    });
});
