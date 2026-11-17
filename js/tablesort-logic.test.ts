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
  clickSortList,
  columnSpec,
  compareKeys,
  compareRows,
  EMPTY_GROUP,
  groupLabel,
  groupRows,
  matchesFilters,
  parseSortList,
  selectOptions,
  sortKey,
  type ColumnSpec,
  type SortKey,
} from './tablesort-logic.ts';

const text: ColumnSpec = { sorter: 'text', grouper: 'none', filter: 'text' };
const unsortable: ColumnSpec = { ...text, sorter: 'none' };

describe('columnSpec', () => {
  it('defaults to sorted, ungrouped text with a text filter', () => {
    assert.deepEqual(columnSpec([]), text);
  });

  it('reads the sorter classes', () => {
    assert.equal(columnSpec(['sorter-false']).sorter, 'none');
    assert.equal(columnSpec(['sorter-digit']).sorter, 'digit');
    assert.equal(columnSpec(['sorter-percent']).sorter, 'percent');
    assert.equal(columnSpec(['sorter-shortDate']).sorter, 'shortDate');
    assert.equal(columnSpec(['sorter-text']).sorter, 'text');
  });

  it('reads the group classes', () => {
    assert.equal(columnSpec(['group-text']).grouper, 'text');
    assert.equal(columnSpec(['group-date-monthyear']).grouper, 'monthyear');
    assert.equal(columnSpec(['group-false']).grouper, 'none');
  });

  it('reads the filter classes', () => {
    assert.equal(columnSpec(['filter-select']).filter, 'select');
    assert.equal(columnSpec(['filter-false']).filter, 'none');
  });

  it('ignores unrelated classes', () => {
    assert.deepEqual(columnSpec(['empty-min', 'number', 'parser-false']), text);
  });
});

describe('parseSortList', () => {
  const specs = [text, text, text];

  it('parses a JSON sort list', () => {
    assert.deepEqual(parseSortList('[[2,1], [0,0]]', specs), [
      [2, 1],
      [0, 0],
    ]);
  });

  it('tolerates stray brackets', () => {
    assert.deepEqual(parseSortList('[[2,0], [1, 0]]]', specs), [
      [2, 0],
      [1, 0],
    ]);
  });

  it('returns no sort for a missing or empty attribute', () => {
    assert.deepEqual(parseSortList(undefined, specs), []);
    assert.deepEqual(parseSortList(null, specs), []);
    assert.deepEqual(parseSortList('', specs), []);
    assert.deepEqual(parseSortList('garbage', specs), []);
  });

  it('drops unknown, unsortable and repeated columns', () => {
    assert.deepEqual(
      parseSortList('[[5,0],[0,1],[1,0],[0,0]]', [text, unsortable]),
      [[0, 1]],
    );
  });
});

describe('sortKey', () => {
  it('keeps text as is, trimmed', () => {
    assert.equal(sortKey('text', '  Name '), 'Name');
  });

  it('parses numbers, ignoring separators and units', () => {
    assert.equal(sortKey('digit', '1,234'), 1234);
    assert.equal(sortKey('digit', '-3.5'), -3.5);
    assert.equal(sortKey('percent', '45.3%'), 45.3);
  });

  it('parses ISO dates as UTC, ignoring a time part', () => {
    assert.equal(sortKey('shortDate', '2021-03-15'), Date.UTC(2021, 2, 15));
    assert.equal(
      sortKey('shortDate', '2021-03-15 10:00:00'),
      Date.UTC(2021, 2, 15),
    );
  });

  it('returns null for empty or unparseable values', () => {
    assert.equal(sortKey('text', '   '), null);
    assert.equal(sortKey('digit', 'n/a'), null);
    assert.equal(sortKey('shortDate', 'never'), null);
  });
});

describe('compareKeys', () => {
  it('compares numbers numerically', () => {
    assert.ok(compareKeys(9, 10) < 0);
  });

  it('compares text naturally and case-insensitively', () => {
    assert.ok(compareKeys('img2', 'img10') < 0);
    assert.equal(compareKeys('Zurich', 'zurich'), 0);
    assert.ok(compareKeys('apple', 'Banana') < 0);
  });
});

describe('compareRows', () => {
  const sort = (rows: SortKey[][], list: [number, 0 | 1][]) =>
    [...rows].sort((a, b) => compareRows(a, b, list));

  it('sorts by several columns', () => {
    const rows: SortKey[][] = [
      ['b', 1],
      ['a', 2],
      ['a', 1],
    ];
    assert.deepEqual(
      sort(rows, [
        [0, 0],
        [1, 1],
      ]),
      [
        ['a', 2],
        ['a', 1],
        ['b', 1],
      ],
    );
  });

  it('puts empty values last in both directions', () => {
    const rows: SortKey[][] = [[null], [2], [1]];
    assert.deepEqual(sort(rows, [[0, 0]]), [[1], [2], [null]]);
    assert.deepEqual(sort(rows, [[0, 1]]), [[2], [1], [null]]);
  });

  it('keeps the original order of equal rows', () => {
    const rows: SortKey[][] = [
      ['x', 'first'],
      ['x', 'second'],
    ];
    assert.deepEqual(sort(rows, [[0, 1]]), rows);
  });

  it('keeps the original order without a sort list', () => {
    const rows: SortKey[][] = [['b'], ['a']];
    assert.deepEqual(sort(rows, []), rows);
  });
});

describe('clickSortList', () => {
  it('sorts a new column ascending, alone', () => {
    assert.deepEqual(
      clickSortList(
        [
          [1, 0],
          [2, 1],
        ],
        0,
        false,
      ),
      [[0, 0]],
    );
  });

  it('toggles the direction of an already sorted column', () => {
    assert.deepEqual(clickSortList([[0, 0]], 0, false), [[0, 1]]);
    assert.deepEqual(clickSortList([[0, 1]], 0, false), [[0, 0]]);
    // A secondary column becomes the only one, with toggled direction.
    assert.deepEqual(
      clickSortList(
        [
          [2, 0],
          [1, 0],
        ],
        1,
        false,
      ),
      [[1, 1]],
    );
  });

  it('adds a column with shift-click', () => {
    assert.deepEqual(clickSortList([[0, 0]], 2, true), [
      [0, 0],
      [2, 0],
    ]);
  });

  it('toggles a column in place with shift-click', () => {
    assert.deepEqual(
      clickSortList(
        [
          [0, 0],
          [2, 0],
        ],
        2,
        true,
      ),
      [
        [0, 0],
        [2, 1],
      ],
    );
  });
});

describe('matchesFilters', () => {
  const kinds = ['text', 'select', 'none'] as const;
  const row = ['zurich trip', 'processed', 'whatever'];

  it('passes everything without filters', () => {
    assert.ok(matchesFilters(row, ['', '', ''], kinds));
  });

  it('matches text filters as substrings', () => {
    assert.ok(matchesFilters(row, ['rich', '', ''], kinds));
    assert.ok(!matchesFilters(row, ['rome', '', ''], kinds));
  });

  it('matches select filters as whole values', () => {
    assert.ok(matchesFilters(row, ['', 'processed', ''], kinds));
    assert.ok(!matchesFilters(row, ['', 'process', ''], kinds));
  });

  it('requires all filters to match', () => {
    assert.ok(!matchesFilters(row, ['zurich', 'raw', ''], kinds));
  });

  it('ignores filters on columns without one', () => {
    assert.ok(matchesFilters(row, ['', '', 'nothing like it'], kinds));
  });
});

describe('groupLabel', () => {
  it('uses the text for text groups', () => {
    assert.equal(groupLabel('text', ' Japan '), 'Japan');
  });

  it('uses month and year for date groups', () => {
    assert.equal(groupLabel('monthyear', '2021-01-04'), 'Jan 2021');
    assert.equal(groupLabel('monthyear', '2024-12-31 23:59'), 'Dec 2024');
  });

  it('labels invalid dates', () => {
    assert.equal(groupLabel('monthyear', 'never'), 'Invalid Date');
  });

  it('labels empty values', () => {
    assert.equal(groupLabel('text', ''), EMPTY_GROUP);
    assert.equal(groupLabel('monthyear', ' '), EMPTY_GROUP);
  });
});

describe('groupRows', () => {
  it('splits rows into runs of equal labels', () => {
    const groups = groupRows(['a1', 'a2', 'b1', 'a3'], (r) => r[0]);
    assert.deepEqual(groups, [
      { label: 'a', rows: ['a1', 'a2'] },
      { label: 'b', rows: ['b1'] },
      { label: 'a', rows: ['a3'] },
    ]);
  });

  it('returns no groups for no rows', () => {
    assert.deepEqual(
      groupRows([], () => 'x'),
      [],
    );
  });
});

describe('selectOptions', () => {
  it('lists distinct values, sorted by label', () => {
    assert.deepEqual(
      selectOptions([
        { value: 'prime', label: 'Prime lens' },
        { value: 'constant-zoom', label: 'Constant-aperture zoom' },
        { value: 'prime', label: 'Prime lens' },
      ]),
      [
        { value: 'constant-zoom', label: 'Constant-aperture zoom' },
        { value: 'prime', label: 'Prime lens' },
      ],
    );
  });

  it('skips empty values and falls back to the value as label', () => {
    assert.deepEqual(
      selectOptions([
        { value: '', label: '' },
        { value: 'raw', label: '' },
      ]),
      [{ value: 'raw', label: 'raw' }],
    );
  });
});
