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
 * Client-side sorting, filtering and grouping of server-rendered tables
 * (`table.sortable`); see tablesort-logic.ts for the column options.
 *
 * Only the first <tbody> is sorted; <thead> gets a filter row, and
 * <tfoot> (e.g. totals) is left alone.
 */

import {
  clickSortList,
  columnSpec,
  compareRows,
  groupLabel,
  groupRows,
  matchesFilters,
  parseSortList,
  selectOptions,
  sortKey,
  type ColumnSpec,
  type SortKey,
  type SortList,
} from './tablesort-logic';

/** Delay before applying a text filter while the user is typing. */
const FILTER_DELAY_MS = 150;

interface Row {
  tr: HTMLTableRowElement;
  /** Per column: `data-text`, or else the text content; lowercase. */
  texts: string[];
  keys: SortKey[];
}

function cellText(cell: HTMLTableCellElement | undefined): string {
  if (cell === undefined) return '';
  return (cell.dataset.text ?? cell.textContent ?? '').trim();
}

function visibleText(cell: HTMLTableCellElement | undefined): string {
  return (cell?.textContent ?? '').replace(/\s+/g, ' ').trim();
}

const SORT_ICONS = {
  none: 'fa-solid fa-sort',
  ascending: 'fa-solid fa-sort-up',
  descending: 'fa-solid fa-sort-down',
};

/** Adds sorting, filtering and grouping to one table. */
export function enhanceTable(table: HTMLTableElement): void {
  const head = table.tHead;
  const body = table.tBodies[0];
  if (head === null || body === undefined || head.rows.length === 0) return;

  const headers = [...head.rows[0].cells];
  const specs: ColumnSpec[] = headers.map((th) => columnSpec(th.classList));
  const rows: Row[] = [...body.rows].map((tr) => {
    const cells = [...tr.cells];
    const texts = specs.map((_, col) => cellText(cells[col]));
    return {
      tr,
      texts: texts.map((t) => t.toLowerCase()),
      keys: specs.map((spec, col) => sortKey(spec.sorter, texts[col])),
    };
  });
  // Group labels keep the case of the cells, so compute them separately.
  const groupTexts = rows.map((r) => [...r.tr.cells].map(cellText));

  let sortList: SortList = parseSortList(table.dataset.sortlist, specs);
  const filters: string[] = specs.map(() => '');

  // Header buttons, so that sorting works from the keyboard too.
  const buttons = headers.map((th, col) => {
    if (specs[col].sorter === 'none') return null;
    const button = document.createElement('button');
    button.type = 'button';
    button.className = 'sort-button';
    button.append(...th.childNodes, ' ');
    const icon = document.createElement('span');
    icon.setAttribute('aria-hidden', 'true');
    button.append(icon);
    button.title = 'Sort (shift-click to add as a secondary sort)';
    button.addEventListener('click', (ev) => {
      sortList = clickSortList(sortList, col, ev.shiftKey);
      render();
    });
    th.append(button);
    return { th, icon };
  });

  // The filter row.
  const filterRow = head.insertRow();
  filterRow.className = 'filter-row';
  specs.forEach((spec, col) => {
    const td = document.createElement('td');
    filterRow.append(td);
    if (spec.filter === 'none') return;
    const name = visibleText(headers[col]);
    if (spec.filter === 'select') {
      const select = document.createElement('select');
      select.className = 'form-select form-select-sm';
      select.setAttribute('aria-label', `Filter by ${name}`);
      select.add(new Option('', ''));
      const opts = selectOptions(
        rows.map((r, i) => ({
          value: r.texts[col],
          label: visibleText(r.tr.cells[col]) || groupTexts[i][col],
        })),
      );
      // Options are created as text, never parsed as HTML.
      for (const { value, label } of opts) select.add(new Option(label, value));
      select.addEventListener('change', () => {
        filters[col] = select.value;
        render();
      });
      td.append(select);
    } else {
      const input = document.createElement('input');
      input.type = 'search';
      input.className = 'form-control form-control-sm';
      input.setAttribute('aria-label', `Filter by ${name}`);
      let timer: ReturnType<typeof setTimeout> | undefined;
      input.addEventListener('input', () => {
        clearTimeout(timer);
        timer = setTimeout(() => {
          filters[col] = input.value.trim().toLowerCase();
          render();
        }, FILTER_DELAY_MS);
      });
      td.append(input);
    }
  });

  const kinds = specs.map((s) => s.filter);

  function render(): void {
    const shown = rows
      .map((row, i) => ({ row, i }))
      .filter(({ row }) => matchesFilters(row.texts, filters, kinds))
      .sort((a, b) => compareRows(a.row.keys, b.row.keys, sortList));

    buttons.forEach((b, col) => {
      if (b === null) return;
      const entry = sortList.find(([c]) => c === col);
      const state =
        entry === undefined
          ? 'none'
          : entry[1] === 0
            ? 'ascending'
            : 'descending';
      b.icon.className = SORT_ICONS[state];
      b.th.setAttribute('aria-sort', state);
    });

    const frag = document.createDocumentFragment();
    const primary = sortList[0]?.[0];
    const grouper = primary === undefined ? 'none' : specs[primary].grouper;
    if (primary === undefined || grouper === 'none') {
      for (const { row } of shown) frag.append(row.tr);
    } else {
      const groups = groupRows(shown, ({ i }) =>
        groupLabel(grouper, groupTexts[i][primary]),
      );
      for (const group of groups) {
        const tr = document.createElement('tr');
        tr.className = 'group-header';
        const th = document.createElement('th');
        th.colSpan = specs.length;
        th.scope = 'rowgroup';
        const count = document.createElement('span');
        count.className = 'group-count';
        count.textContent = `(${group.rows.length})`;
        th.append(group.label, ' ', count);
        tr.append(th);
        frag.append(tr);
        for (const { row } of group.rows) frag.append(row.tr);
      }
    }
    body.replaceChildren(frag);
  }

  render();
}

/** Enhances all sortable tables on the page, once the DOM is ready. */
export function setupSortableTables(): void {
  const run = () =>
    document
      .querySelectorAll<HTMLTableElement>('table.sortable')
      .forEach(enhanceTable);
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', run, { once: true });
  } else {
    run();
  }
}
