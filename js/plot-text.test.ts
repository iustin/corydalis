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

import { escapePlotText, escapeTraces } from './plot-text.ts';

describe('escapePlotText', () => {
  it('escapes the characters Plotly reads as markup', () => {
    assert.equal(
      escapePlotText('<a href="https://evil.example">Canon</a>'),
      '&lt;a href="https://evil.example"&gt;Canon&lt;/a&gt;',
    );
  });
  it('escapes ampersands first, so entities show as typed', () => {
    assert.equal(escapePlotText('A&amp;B <br>'), 'A&amp;amp;B &lt;br&gt;');
  });
  it('keeps other text as it is', () => {
    assert.equal(escapePlotText('EF 24-70mm f/2.8L "II"'), 'EF 24-70mm f/2.8L "II"');
  });
});

describe('escapeTraces', () => {
  it('escapes the displayed text of each trace', () => {
    const [trace] = escapeTraces([
      {
        name: '<b>Nikon</b>',
        x: ['<i>a</i>', 'b'],
        y: [1, 2],
        text: '<br>',
        labels: ['<s>x</s>'],
        hovertext: ['<u>h</u>'],
      },
    ]);
    assert.deepEqual(trace, {
      name: '&lt;b&gt;Nikon&lt;/b&gt;',
      x: ['&lt;i&gt;a&lt;/i&gt;', 'b'],
      y: [1, 2],
      text: '&lt;br&gt;',
      labels: ['&lt;s&gt;x&lt;/s&gt;'],
      hovertext: ['&lt;u&gt;h&lt;/u&gt;'],
    });
  });
  it('keeps the other fields, and numbers, as they are', () => {
    const trace = {
      type: 'bar',
      hovertemplate: '%{label}: %{y}<extra></extra>',
      x: [2020, 2021],
      marker: { color: '<red>' },
    };
    assert.deepEqual(escapeTraces([trace]), [trace]);
  });
  it("doesn't change the given traces", () => {
    const trace = { name: '<b>x</b>', x: ['<i>y</i>'] };
    escapeTraces([trace]);
    assert.deepEqual(trace, { name: '<b>x</b>', x: ['<i>y</i>'] });
  });
});
