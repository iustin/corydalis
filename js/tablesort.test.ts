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
import { describe, it, type TestContext } from 'node:test';

import { JSDOM } from 'jsdom';

import { enhanceTable, setupSortableTables } from './tablesort.ts';

/** A table like the folder list: icon, name, date, count, text, status. */
const FOLDERS = `
<table class="sortable" data-sortlist="[[2,0],[1,0]]">
  <thead>
    <tr>
      <th class="sorter-false filter-false">Icon</th>
      <th class="sorter-text">Name</th>
      <th class="sorter-shortDate group-date-monthyear">First picture</th>
      <th class="sorter-digit">Images</th>
      <th class="group-text">Country</th>
      <th class="filter-select">Status</th>
    </tr>
  </thead>
  <tbody>
    <tr><td><span class="icon"></span></td><td><a href="/b">f-b</a></td>
      <td>2021-01-04</td><td>6</td><td>Japan</td>
      <td data-text="raw"><b>Raw only</b></td></tr>
    <tr><td><span class="icon"></span></td><td><a href="/a">f-a</a></td>
      <td>2021-01-01</td><td>12</td><td>Switzerland</td>
      <td data-text="processed"><b>Processed</b></td></tr>
    <tr><td><span class="icon"></span></td><td><a href="/c">f-c</a></td>
      <td>2021-03-05</td><td>120</td><td>Japan</td>
      <td data-text="processed"><b>Processed</b></td></tr>
    <tr><td><span class="icon"></span></td><td><a href="/d">f-d</a></td>
      <td>2021-01-20</td><td></td><td>Norway</td>
      <td data-text="raw"><b>Raw only</b></td></tr>
  </tbody>
  <tfoot>
    <tr><td colspan="3">Totals</td><td>138</td><td></td><td></td></tr>
  </tfoot>
</table>`;

/** Creates a document holding the given HTML. */
function page(html: string): Document {
  return new JSDOM(html).window.document;
}

/** Creates and enhances the first table in the given HTML. */
function enhanced(html = FOLDERS): HTMLTableElement {
  const table = page(html).querySelector('table') as HTMLTableElement;
  enhanceTable(table);
  return table;
}

const dataRows = (table: HTMLTableElement) =>
  [...table.tBodies[0].rows].filter((r) => r.className !== 'group-header');

/** The names (second column) of the shown rows, in order. */
const names = (table: HTMLTableElement) =>
  dataRows(table).map((r) => r.cells[1].textContent?.trim());

/** The texts of the group header rows, in order. */
const groups = (table: HTMLTableElement) =>
  [...table.querySelectorAll('tr.group-header')].map((r) =>
    r.textContent?.trim(),
  );

const ariaSort = (table: HTMLTableElement) =>
  [...table.tHead!.rows[0].cells].map((th) => th.getAttribute('aria-sort'));

const header = (table: HTMLTableElement, col: number) =>
  table.tHead!.rows[0].cells[col].querySelector('button')!;

function shiftClick(button: HTMLButtonElement) {
  const view = button.ownerDocument.defaultView!;
  button.dispatchEvent(
    new view.MouseEvent('click', { shiftKey: true, bubbles: true }),
  );
}

const filterInput = (table: HTMLTableElement, col: number) =>
  table.querySelector<HTMLInputElement>(
    `tr.filter-row td:nth-child(${col + 1}) input`,
  )!;

/** Types into a text filter and lets its delay pass (on mocked timers). */
function typeFilter(
  t: TestContext,
  table: HTMLTableElement,
  col: number,
  value: string,
) {
  const input = filterInput(table, col);
  input.value = value;
  input.dispatchEvent(new input.ownerDocument.defaultView!.Event('input'));
  t.mock.timers.tick(1000);
}

describe('enhanceTable', () => {
  it('applies the initial sort list and groups by the first column', () => {
    const table = enhanced();
    assert.deepEqual(names(table), ['f-a', 'f-b', 'f-d', 'f-c']);
    assert.deepEqual(groups(table), ['Jan 2021 (3)', 'Mar 2021 (1)']);
  });

  it('shows the group count separately from the label', () => {
    const count = enhanced().querySelector('tr.group-header .group-count');
    assert.equal(count?.textContent, '(3)');
  });

  it('makes sortable headers buttons, keeping their text', () => {
    const table = enhanced();
    const button = header(table, 1);
    assert.equal(button.type, 'button');
    assert.match(button.textContent ?? '', /^Name/);
    assert.equal(table.tHead!.rows[0].cells[0].querySelector('button'), null);
  });

  it('sets aria-sort on the sortable headers', () => {
    assert.deepEqual(ariaSort(enhanced()), [
      null,
      'ascending',
      'ascending',
      'none',
      'none',
      'none',
    ]);
  });

  it('sorts numerically on click, with empty cells last', () => {
    const table = enhanced();
    header(table, 3).click();
    assert.deepEqual(names(table), ['f-b', 'f-a', 'f-c', 'f-d']);
    assert.deepEqual(groups(table), []);
    header(table, 3).click();
    assert.deepEqual(names(table), ['f-c', 'f-a', 'f-b', 'f-d']);
    assert.equal(ariaSort(table)[3], 'descending');
    assert.equal(ariaSort(table)[2], 'none');
  });

  it('groups by text columns, and adds a sort with shift-click', () => {
    const table = enhanced();
    header(table, 4).click();
    assert.deepEqual(groups(table), [
      'Japan (2)',
      'Norway (1)',
      'Switzerland (1)',
    ]);
    shiftClick(header(table, 1));
    shiftClick(header(table, 1));
    assert.deepEqual(names(table), ['f-c', 'f-b', 'f-d', 'f-a']);
    assert.equal(ariaSort(table)[1], 'descending');
    assert.equal(ariaSort(table)[4], 'ascending');
  });

  it('applies text filters after a delay, ignoring case', (t) => {
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const table = enhanced();
    const input = filterInput(table, 4);
    input.value = 'JAP';
    input.dispatchEvent(new input.ownerDocument.defaultView!.Event('input'));
    assert.equal(names(table).length, 4, 'not applied right away');
    t.mock.timers.tick(1000);
    assert.deepEqual(names(table), ['f-b', 'f-c']);
    assert.deepEqual(groups(table), ['Jan 2021 (1)', 'Mar 2021 (1)']);
    typeFilter(t, table, 4, '');
    assert.equal(names(table).length, 4);
  });

  it('combines the filters of all columns', (t) => {
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const table = enhanced();
    typeFilter(t, table, 4, 'japan');
    typeFilter(t, table, 1, 'f-c');
    assert.deepEqual(names(table), ['f-c']);
  });

  it('compares numbers in numeric filters', (t) => {
    t.mock.timers.enable({ apis: ['setTimeout'] });
    const table = enhanced();
    typeFilter(t, table, 3, '>10');
    assert.deepEqual(names(table), ['f-a', 'f-c']);
    typeFilter(t, table, 3, '!=6');
    assert.deepEqual(names(table), ['f-a', 'f-c'], 'empty cell excluded');
    typeFilter(t, table, 3, '12');
    assert.deepEqual(names(table), ['f-a', 'f-c'], 'bare number: substring');
  });

  it('hints at the operators on numeric filters only', () => {
    const table = enhanced();
    assert.equal(filterInput(table, 3).placeholder, 'e.g. >10');
    assert.equal(filterInput(table, 1).placeholder, '');
  });

  it('has no filter on columns with filter-false', () => {
    const cell = enhanced().querySelector('tr.filter-row td');
    assert.equal(cell?.childElementCount, 0);
  });

  it('builds select filters from data-text, labelled by the cell text', () => {
    const table = enhanced();
    const select = table.querySelector<HTMLSelectElement>(
      'tr.filter-row select',
    )!;
    const options = [...select.querySelectorAll('option')].map((o) => [
      o.value,
      o.textContent,
    ]);
    assert.deepEqual(options, [
      ['', ''],
      ['processed', 'Processed'],
      ['raw', 'Raw only'],
    ]);
    select.value = 'raw';
    select.dispatchEvent(new select.ownerDocument.defaultView!.Event('change'));
    assert.deepEqual(names(table), ['f-b', 'f-d']);
  });

  it('creates select options as text, never as HTML', () => {
    const table = enhanced(`
      <table class="sortable"><thead><tr>
        <th class="filter-select">File</th>
      </tr></thead><tbody>
        <tr><td>&lt;img src=x onerror=alert(1)&gt;</td></tr>
      </tbody></table>`);
    const option = table.querySelectorAll('option')[1];
    assert.equal(option.textContent, '<img src=x onerror=alert(1)>');
    assert.equal(option.childElementCount, 0);
  });

  it('leaves the footer alone', () => {
    const table = enhanced();
    header(table, 3).click();
    assert.equal(table.tFoot?.rows.length, 1);
    assert.match(table.tFoot?.textContent ?? '', /Totals\s*138/);
  });

  it('keeps the server order without a valid sort list', () => {
    const table = enhanced(FOLDERS.replace('[[2,0],[1,0]]', '[[2,0]]]'));
    assert.deepEqual(names(table), ['f-b', 'f-a', 'f-c', 'f-d']);
    assert.deepEqual(groups(table), []);
  });

  it('ignores tables without a header or body', () => {
    const doc = page('<table class="sortable"><tr><td>x</td></tr></table>');
    const table = doc.querySelector('table') as HTMLTableElement;
    const before = table.outerHTML;
    enhanceTable(table);
    assert.equal(table.outerHTML, before);
  });
});

describe('setupSortableTables', () => {
  const twoTables = () =>
    page(FOLDERS + FOLDERS.replace('class="sortable"', ''));
  const buttons = (doc: Document) =>
    [...doc.querySelectorAll('table')].map(
      (t) => t.querySelectorAll('.sort-button').length,
    );
  const loaded = (doc: Document) =>
    new Promise((resolve) =>
      doc.addEventListener('DOMContentLoaded', resolve, { once: true }),
    );

  it('waits for the DOM while the page is loading', async () => {
    // A document reopened with document.open() is loading again, as a
    // page still being parsed, until document.close() ends it (firing
    // DOMContentLoaded); a newly created document may already be past
    // loading (it is, since jsdom 31).
    const doc = page('');
    doc.open();
    doc.write(FOLDERS + FOLDERS.replace('class="sortable"', ''));
    assert.equal(doc.readyState, 'loading');
    const ready = loaded(doc);
    setupSortableTables(doc);
    assert.deepEqual(buttons(doc), [0, 0]);
    doc.close();
    await ready;
    assert.deepEqual(buttons(doc), [5, 0], 'only the sortable table');
  });

  it('enhances the tables right away once the DOM is ready', async () => {
    const doc = twoTables();
    await loaded(doc);
    setupSortableTables(doc);
    assert.deepEqual(buttons(doc), [5, 0]);
  });
});
