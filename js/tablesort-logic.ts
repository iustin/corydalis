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

/*
 * The DOM-independent part of the table sorting/filtering/grouping
 * code (see tablesort.ts), kept separate so that it can be unit-tested
 * without a browser.
 *
 * Columns are configured via classes on their header cells, using the
 * names of the tablesorter plugin this replaces:
 *
 * - `sorter-text` (the default), `sorter-digit`, `sorter-percent`,
 *   `sorter-shortDate`, or `sorter-false` for unsortable columns;
 * - `group-text` or `group-date-monthyear` to show group header rows
 *   when the table is sorted primarily by that column; columns without
 *   a `group-*` class (or with `group-false`) are not grouped;
 * - `filter-select` for a drop-down filter, `filter-false` for none;
 *   the default is a free-text filter.
 *
 * Cells can carry a `data-text` attribute, used instead of their text
 * content for sorting, filtering and grouping.
 */

/** How a column's values are compared. */
export type Sorter = 'text' | 'digit' | 'percent' | 'shortDate' | 'none';

/** How a column's rows are grouped when it's the primary sort column. */
export type Grouper = 'text' | 'monthyear' | 'none';

/** Which filter control a column has. */
export type FilterKind = 'text' | 'select' | 'none';

export interface ColumnSpec {
  sorter: Sorter;
  grouper: Grouper;
  filter: FilterKind;
}

/** Sort direction: 0 is ascending, 1 is descending (as in tablesorter). */
export type Direction = 0 | 1;

/** A sort order: (column index, direction) pairs, most significant first. */
export type SortList = [number, Direction][];

/**
 * A sort key. `null` marks empty or unparseable values, which always
 * sort last, regardless of the direction.
 */
export type SortKey = string | number | null;

/** Returns the column configuration for a header with the given classes. */
export function columnSpec(classes: Iterable<string>): ColumnSpec {
  const cls = new Set(classes);
  let sorter: Sorter = 'text';
  if (cls.has('sorter-false')) sorter = 'none';
  else if (cls.has('sorter-digit')) sorter = 'digit';
  else if (cls.has('sorter-percent')) sorter = 'percent';
  else if (cls.has('sorter-shortDate')) sorter = 'shortDate';

  let grouper: Grouper = 'none';
  if (cls.has('group-text')) grouper = 'text';
  else if (cls.has('group-date-monthyear')) grouper = 'monthyear';

  let filter: FilterKind = 'text';
  if (cls.has('filter-false')) filter = 'none';
  else if (cls.has('filter-select')) filter = 'select';

  return { sorter, grouper, filter };
}

/**
 * Parses a `data-sortlist` attribute such as `[[2,0],[1,0]]`.
 *
 * This is deliberately lenient (as tablesorter was): it picks the
 * `[column, direction]` pairs out of the text, so stray brackets don't
 * lose the whole sort order. Pairs naming columns outside `0..columns-1`
 * or unsortable columns are dropped, as are repeated columns.
 */
export function parseSortList(
  attr: string | null | undefined,
  specs: readonly ColumnSpec[],
): SortList {
  const result: SortList = [];
  if (!attr) return result;
  for (const m of attr.matchAll(/\[\s*(\d+)\s*,\s*([01])\s*\]/g)) {
    const col = Number(m[1]);
    const spec = specs[col];
    if (
      spec === undefined ||
      spec.sorter === 'none' ||
      result.some(([c]) => c === col)
    ) {
      continue;
    }
    result.push([col, m[2] === '1' ? 1 : 0]);
  }
  return result;
}

/** Matches dates in the YYYY-MM-DD form, optionally followed by a time. */
const ISO_DATE = /^(\d{4})-(\d{2})-(\d{2})/;

/** Returns the sort key of a cell's text, for the given sorter. */
export function sortKey(sorter: Sorter, text: string): SortKey {
  const t = text.trim();
  if (t === '') return null;
  switch (sorter) {
    case 'digit':
    case 'percent': {
      // Allow thousands separators and units, e.g. "1,234" or "45.3%".
      const n = Number.parseFloat(t.replace(/[^0-9.eE+-]/g, ''));
      return Number.isNaN(n) ? null : n;
    }
    case 'shortDate': {
      const m = ISO_DATE.exec(t);
      const ms = m
        ? Date.UTC(Number(m[1]), Number(m[2]) - 1, Number(m[3]))
        : Date.parse(t);
      return Number.isNaN(ms) ? null : ms;
    }
    default:
      return t;
  }
}

const collator = new Intl.Collator(undefined, {
  numeric: true,
  sensitivity: 'base',
});

/**
 * Compares two keys in ascending order. Text is compared naturally
 * ("img2" before "img10") and case-insensitively.
 */
export function compareKeys(a: SortKey, b: SortKey): number {
  if (typeof a === 'number' && typeof b === 'number') return a - b;
  return collator.compare(String(a), String(b));
}

/**
 * Compares two rows (given as their per-column sort keys) by a sort
 * list. Empty keys sort last in either direction. Ties keep the
 * original order, as Array.prototype.sort is stable.
 */
export function compareRows(
  a: readonly SortKey[],
  b: readonly SortKey[],
  sortList: SortList,
): number {
  for (const [col, dir] of sortList) {
    const ka = a[col] ?? null;
    const kb = b[col] ?? null;
    if (ka === null || kb === null) {
      if (ka === kb) continue;
      return ka === null ? 1 : -1;
    }
    const c = compareKeys(ka, kb);
    if (c !== 0) return dir === 1 ? -c : c;
  }
  return 0;
}

/**
 * Returns the new sort list after a click on a column header.
 *
 * A plain click sorts by that column alone: ascending if it wasn't the
 * only sort column, and toggling the direction if it was. With
 * `add` (shift-click), the column is added to the sort list as
 * ascending, or its direction toggled if it's already in it.
 */
export function clickSortList(
  current: SortList,
  col: number,
  add: boolean,
): SortList {
  const idx = current.findIndex(([c]) => c === col);
  const toggled = (d: Direction): Direction => (d === 0 ? 1 : 0);
  if (add) {
    if (idx < 0) return [...current, [col, 0]];
    return current.map(([c, d]) => [c, c === col ? toggled(d) : d]);
  }
  if (idx >= 0) return [[col, toggled(current[idx][1])]];
  return [[col, 0]];
}

/**
 * Returns whether a row (given as its per-column texts) passes the
 * filters. Filters are lowercase; an empty filter passes everything.
 * Text filters match substrings, select filters whole values, both
 * case-insensitively.
 */
export function matchesFilters(
  texts: readonly string[],
  filters: readonly string[],
  kinds: readonly FilterKind[],
): boolean {
  return filters.every((f, col) => {
    if (f === '' || kinds[col] === 'none') return true;
    const t = (texts[col] ?? '').toLowerCase();
    return kinds[col] === 'select' ? t === f : t.includes(f);
  });
}

const MONTHS = [
  'Jan',
  'Feb',
  'Mar',
  'Apr',
  'May',
  'Jun',
  'Jul',
  'Aug',
  'Sep',
  'Oct',
  'Nov',
  'Dec',
];

/** Label of the group for rows whose grouping text is empty. */
export const EMPTY_GROUP = '(none)';

/** Returns the group label of a cell's text, for the given grouper. */
export function groupLabel(grouper: Grouper, text: string): string {
  const t = text.trim();
  if (t === '') return EMPTY_GROUP;
  if (grouper !== 'monthyear') return t;
  const m = ISO_DATE.exec(t);
  if (m) return `${MONTHS[Number(m[2]) - 1] ?? '?'} ${m[1]}`;
  const d = new Date(t);
  return Number.isNaN(d.getTime())
    ? 'Invalid Date'
    : `${MONTHS[d.getMonth()]} ${d.getFullYear()}`;
}

/** A run of consecutive rows with the same group label. */
export interface Group<T> {
  label: string;
  rows: T[];
}

/**
 * Splits (already sorted) rows into runs of equal group labels. Equal
 * labels that aren't adjacent form separate groups, as in the display.
 */
export function groupRows<T>(
  rows: readonly T[],
  label: (row: T) => string,
): Group<T>[] {
  const groups: Group<T>[] = [];
  let last: Group<T> | undefined;
  for (const row of rows) {
    const l = label(row);
    if (last === undefined || last.label !== l) {
      last = { label: l, rows: [] };
      groups.push(last);
    }
    last.rows.push(row);
  }
  return groups;
}

/**
 * Returns the options of a select filter: the distinct values of a
 * column, sorted, each with the label to show (the first text shown in
 * a cell with that value, which differs from the value when cells use
 * `data-text`).
 */
export function selectOptions(
  values: readonly { value: string; label: string }[],
): { value: string; label: string }[] {
  const seen = new Map<string, string>();
  for (const { value, label } of values) {
    if (value !== '' && !seen.has(value)) seen.set(value, label || value);
  }
  return [...seen]
    .map(([value, label]) => ({ value, label }))
    .sort((a, b) => collator.compare(a.label, b.label));
}
