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

import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import {
  Cory,
  DOUBLE_TAP_THRESHOLD,
  Dimensions,
  type ImageInfo,
  type LayoutState,
  MAX_ZOOM,
  MOVIE_MESSAGE,
  TAP_DURATION_THRESHOLD,
  TAP_MOVEMENT_THRESHOLD,
  type ViewInfo,
  classifyPointerUp,
  clampZoom,
  getDistanceBetweenPoints,
  getMidpoint,
  imageUrlScaled,
  layoutImage,
  limitNumber,
  movieMessage,
  seekPercent,
  seekTime,
  wheelZoomRatio,
} from './viewer-logic.ts';

const d = (x: number, y: number) => new Dimensions(x, y);

/** Compares dimensions by value (and not by class). */
function assertDims(actual: Dimensions, x: number, y: number) {
  // Adding 0 turns -0 into 0, which deepEqual tells apart.
  assert.deepEqual({ x: actual.x + 0, y: actual.y + 0 }, { x, y });
}

describe('Dimensions', () => {
  it('clones into a separate object', () => {
    const a = d(1, 2);
    const b = a.clone();
    b.x = 5;
    assertDims(a, 1, 2);
    assertDims(b, 5, 2);
  });
  it('returns the longest side', () => {
    assert.equal(d(3, 2).longest(), 3);
    assert.equal(d(2, 3).longest(), 3);
    assert.equal(d(2, 2).longest(), 2);
  });
  it('computes the absolute magnitude', () => {
    assert.equal(d(-3, 4).absMagnitude(), 7);
    assert.equal(d(0, 0).absMagnitude(), 0);
  });
  it('swaps, negates and scales (rounding)', () => {
    assertDims(d(1, 2).swapped(), 2, 1);
    assertDims(d(1, -2).negated(), -1, 2);
    assertDims(d(3, 5).scaled(0.5), 2, 3);
    assertDims(d(10, 20).scaled(2), 20, 40);
  });
  it('compares by value', () => {
    assert.ok(d(1, 2).equals(d(1, 2)));
    assert.ok(!d(1, 2).equals(d(2, 1)));
    assert.ok(!d(1, 2).equals(d(1, 3)));
  });
  it('divides, adds and subtracts per dimension', () => {
    assertDims(d(10, 9).dividedBy(d(2, 3)), 5, 3);
    assertDims(d(10, 9).minus(d(2, 3)), 8, 6);
    assertDims(d(10, 9).plus(d(2, 3)), 12, 12);
  });
  it('clamps by numbers', () => {
    assertDims(d(-1, 5).clampMin(0), 0, 5);
    assertDims(d(5, -1).clampMin(0), 5, 0);
    assertDims(d(-1, 5).clampMax(2), -1, 2);
    assertDims(d(5, -1).clampMax(2), 2, -1);
  });
  it('clamps by dimensions', () => {
    assertDims(d(1, 1).clampMin(d(2, 0)), 2, 1);
    assertDims(d(1, 1).clampMin(d(0, 2)), 1, 2);
    assertDims(d(3, 3).clampMax(d(2, 4)), 2, 3);
    assertDims(d(3, 3).clampMax(d(4, 2)), 3, 2);
  });
  it('clamps between a boundary and its negation', () => {
    assertDims(d(-10, 10).clampLoHi(d(5, 3)), -5, 3);
    assertDims(d(1, -1).clampLoHi(d(5, 3)), 1, -1);
    assertDims(d(4, 4).clampLoHi(d(0, 0)), 0, 0);
  });
});

describe('point helpers', () => {
  it('computes distances', () => {
    assert.equal(getDistanceBetweenPoints(d(0, 0), d(3, 4)), 5);
    assert.equal(getDistanceBetweenPoints(d(1, 1), d(1, 1)), 0);
  });
  it('computes midpoints', () => {
    assertDims(getMidpoint(d(0, 0), d(4, 2)), 2, 1);
    assertDims(getMidpoint(d(-2, 1), d(2, 2)), 0, 1.5);
  });
  it('limits numbers', () => {
    assert.equal(limitNumber(0, 10, -5), 0);
    assert.equal(limitNumber(0, 10, 15), 10);
    assert.equal(limitNumber(0, 10, 5), 5);
  });
});

function image(name: string, extra: Partial<ImageInfo> = {}): ImageInfo {
  const url = (kind: string) => `https://cory.example/${kind}/${name}`;
  return {
    info: url('info'),
    bytes: url('bytes'),
    view: url('view'),
    flag: url('flag'),
    list: url('list'),
    browse: url('browse'),
    name,
    transform: [0, false, false],
    matrix: [1, 0, 0, 1],
    fullres: false,
    exifhtml: '',
    ...extra,
  };
}

function viewInfo(): ViewInfo {
  return {
    year: '2020',
    yearurl: '/y',
    folder: 'f',
    folderurl: '/f',
    folderlist: '/fl',
    image: 'a',
    imageurl: '/a',
    first: image('first'),
    current: image('a', {
      transform: [90, true, false],
      matrix: [0, 1, -1, 0],
    }),
    last: image('last'),
  };
}

function newCory(devicePixelRatio = 1): Cory {
  let n = 0;
  return new Cory(viewInfo(), {
    url: 'https://cory.example/view/a',
    devicePixelRatio,
    newImage: () => ({ id: n++ }) as unknown as HTMLImageElement,
  });
}

describe('Cory', () => {
  it('starts from the boot info and environment', () => {
    const cory = newCory(2);
    assert.equal(cory.info.image, 'a');
    assert.deepEqual(cory.state.transform, [90, true, false]);
    assert.deepEqual(cory.state.matrix, [0, 1, -1, 0]);
    assert.equal(cory.state.url, 'https://cory.example/view/a');
    assert.equal(cory.state.devicePixelRatio, 2);
    assert.equal(cory.state.scale, 1);
    assert.equal(cory.state.video, null);
    assert.equal(cory.state.fullscreen, false);
    // Three separate images: previous, next, and current.
    const ids = [cory.prev, cory.next, cory.state.img].map(
      (i) => (i as unknown as { id: number }).id,
    );
    assert.deepEqual(ids.sort((x, y) => x - y), [0, 1, 2]);
  });

  it('pans within the limits, scaled by the pixel ratio', () => {
    const cory = newCory(2);
    cory.state.panLimits = d(10, 5);
    assert.equal(cory.panX(3), true);
    assert.equal(cory.state.panOffsets.x, 6);
    assert.equal(cory.panX(10), true);
    assert.equal(cory.state.panOffsets.x, 10);
    // At the limit, nothing changes.
    assert.equal(cory.panX(1), false);
    assert.equal(cory.panY(-10), true);
    assert.equal(cory.state.panOffsets.y, -5);
    assert.equal(cory.panY(-1), false);
  });

  it('pans both axes, reporting any change', () => {
    const cory = newCory();
    cory.state.panLimits = d(10, 0);
    assert.equal(cory.pan(1, 1), true);
    assertDims(cory.state.panOffsets, 1, 0);
    cory.state.panLimits = d(0, 10);
    cory.state.panOffsets = d(0, 0);
    assert.equal(cory.pan(1, 1), true);
    assertDims(cory.state.panOffsets, 0, 1);
    cory.state.panLimits = d(0, 0);
    cory.state.panOffsets = d(0, 0);
    assert.equal(cory.pan(1, 1), false);
  });

  it('converts canvas coordinates to CSS ones', () => {
    const cory = newCory(2);
    cory.state.canvasSize = d(800, 600);
    // The canvas centre is the middle of the display.
    assertDims(cory.convertToCss(d(0, 0)), 200, 150);
    assertDims(cory.convertToCss(d(-400, -300)), 0, 0);
  });
});

/** A layout state for a canvas of the given size. */
function layoutState(
  canvas: Dimensions,
  extra: Partial<LayoutState> = {},
): LayoutState {
  return {
    canvasSize: canvas,
    imageSize: d(0, 0),
    scale11: 1,
    scale: 1,
    lastScale: 1,
    panLimits: d(0, 0),
    panOffsets: d(0, 0),
    devicePixelRatio: 1,
    ...extra,
  };
}

describe('layoutImage', () => {
  it('fits and centres an image in the canvas', () => {
    const state = layoutState(d(800, 600));
    const layout = layoutImage(state, d(1600, 1000), 0, null);
    assertDims(layout.origin, 400, 300);
    assertDims(layout.size, 800, 500);
    assertDims(layout.offsets, -400, -250);
    assertDims(state.imageSize, 1600, 1000);
    assert.equal(state.scale11, 2);
    assertDims(state.panLimits, 0, 0);
    assertDims(state.panOffsets, 0, 0);
  });

  it('swaps the sides of rotated images', () => {
    const state = layoutState(d(800, 600));
    const layout = layoutImage(state, d(1200, 600), 90, null);
    assertDims(state.imageSize, 600, 1200);
    assert.equal(state.scale11, 2);
    // The drawn size is still in the image's own coordinates.
    assertDims(layout.size, 600, 300);
    assertDims(layout.offsets, -300, -150);
  });

  it('computes the pan limits when zoomed in', () => {
    const state = layoutState(d(800, 600), { scale: 2 });
    const layout = layoutImage(state, d(800, 600), 0, null);
    assertDims(layout.size, 1600, 1200);
    assertDims(state.panLimits, 400, 300);
    assertDims(layout.offsets, -800, -600);
    assert.equal(state.lastScale, 2);
  });

  it('scales the pan offsets with the limits', () => {
    const state = layoutState(d(800, 600), {
      scale: 3,
      lastScale: 2,
      panLimits: d(400, 300),
      panOffsets: d(200, -100),
    });
    const layout = layoutImage(state, d(800, 600), 0, null);
    assertDims(state.panLimits, 800, 600);
    assertDims(state.panOffsets, 400, -200);
    assertDims(layout.offsets, -1200 + 400, -900 - 200);
  });

  it('keeps the pan offsets within the limits', () => {
    const state = layoutState(d(800, 600), {
      panOffsets: d(50, -50),
    });
    layoutImage(state, d(800, 600), 0, null);
    assertDims(state.panOffsets, 0, 0);
  });

  it('zooms around the given point', () => {
    const state = layoutState(d(800, 600), { scale: 2, lastScale: 1 });
    const log: unknown[][] = [];
    // Zooming around the centre keeps the centre.
    layoutImage(state, d(800, 600), 0, d(400, 300), (...args) =>
      log.push(args),
    );
    assertDims(state.panOffsets, 0, 0);
    assert.ok(log.length > 0);
    // Zooming around the top left corner keeps that corner in place.
    const corner = layoutState(d(800, 600), { scale: 2, lastScale: 1 });
    layoutImage(corner, d(800, 600), 0, d(0, 0));
    assertDims(corner.panOffsets, 400, 300);
  });

  it('zooms around a point in CSS pixels', () => {
    const state = layoutState(d(800, 600), {
      scale: 2,
      lastScale: 1,
      devicePixelRatio: 2,
    });
    layoutImage(state, d(800, 600), 0, d(0, 0));
    assertDims(state.panOffsets, 400, 300);
  });
});

describe('clampZoom', () => {
  it('keeps zooms in range', () => {
    assert.deepEqual(clampZoom(2, 1), { scale: 2, message: null });
    assert.deepEqual(clampZoom(MAX_ZOOM, 1), {
      scale: MAX_ZOOM,
      message: null,
    });
  });
  it('turns the initial zero zoom into 1', () => {
    assert.deepEqual(clampZoom(0, 2), { scale: 1, message: null });
  });
  it('stops at the minimum zoom, saying so only once there', () => {
    assert.deepEqual(clampZoom(0.5, 2), { scale: 1, message: null });
    const { scale, message } = clampZoom(0.9, 1);
    assert.equal(scale, 1);
    assert.match(message ?? '', /minimum/);
  });
  it('stops at the maximum zoom, saying so', () => {
    const { scale, message } = clampZoom(MAX_ZOOM * 1.1, MAX_ZOOM);
    assert.equal(scale, MAX_ZOOM);
    assert.match(message ?? '', /[Mm]aximum/);
  });
});

describe('imageUrlScaled', () => {
  it('asks for the longest canvas side', () => {
    assert.equal(
      imageUrlScaled('https://cory.example/bytes/a', d(800, 1200)),
      'https://cory.example/bytes/a?res=1200',
    );
  });
  it('keeps other parameters, and replaces an earlier resolution', () => {
    const url = new URL(
      imageUrlScaled('https://cory.example/bytes/a?q=x&res=10', d(800, 600)),
    );
    assert.equal(url.searchParams.get('q'), 'x');
    assert.deepEqual(url.searchParams.getAll('res'), ['800']);
  });
});

describe('movieMessage', () => {
  it('mentions movies only', () => {
    assert.equal(movieMessage({ movie: '/m' }), MOVIE_MESSAGE);
    assert.equal(movieMessage({}), '');
  });
});

describe('wheelZoomRatio', () => {
  it('zooms in on scrolling down, and out on scrolling up', () => {
    assert.equal(wheelZoomRatio(0, 4), 1.04);
    assert.equal(wheelZoomRatio(0, -4), 1 / 1.04);
  });
  it('caps fast scrolls', () => {
    assert.equal(wheelZoomRatio(0, 300), 1.1);
    assert.equal(wheelZoomRatio(0, -300), 1 / 1.1);
  });
  it('ignores mostly horizontal scrolls', () => {
    assert.equal(wheelZoomRatio(5, 0), null);
    assert.equal(wheelZoomRatio(5, 10), null);
    assert.equal(wheelZoomRatio(4, 10), 1.1);
  });
});

describe('classifyPointerUp', () => {
  const short = TAP_DURATION_THRESHOLD - 1;
  const far = TAP_MOVEMENT_THRESHOLD + 1;
  const later = DOUBLE_TAP_THRESHOLD;
  it('treats long gestures as pans', () => {
    assert.equal(
      classifyPointerUp(TAP_DURATION_THRESHOLD, d(0, 0), later, 1),
      null,
    );
    assert.equal(
      classifyPointerUp(TAP_DURATION_THRESHOLD, d(far, 0), later, 1),
      null,
    );
  });
  it('detects horizontal swipes', () => {
    assert.equal(classifyPointerUp(short, d(far, 0), later, 1), 'previous');
    assert.equal(classifyPointerUp(short, d(-far, 10), later, 1), 'next');
  });
  it('ignores vertical swipes', () => {
    assert.equal(
      classifyPointerUp(short, d(0, far), later, 1),
      'vertical-swipe',
    );
    assert.equal(
      classifyPointerUp(short, d(-far, far), later, 1),
      'vertical-swipe',
    );
  });
  it('treats swipes as pans when zoomed in', () => {
    assert.equal(classifyPointerUp(short, d(far, 0), later, 2), null);
  });
  it('detects taps and double taps', () => {
    assert.equal(classifyPointerUp(short, d(10, 10), later, 1), 'tap');
    assert.equal(
      classifyPointerUp(short, d(10, 10), DOUBLE_TAP_THRESHOLD - 1, 2),
      'double-tap',
    );
  });
});

describe('seek bar', () => {
  it('converts between times and percentages', () => {
    assert.equal(seekPercent(5, 20), 25);
    assert.equal(seekTime(20, 25), 5);
    assert.equal(seekTime(20, seekPercent(7, 20)), 7);
  });
});
