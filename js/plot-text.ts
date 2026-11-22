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

// Plotly reads a subset of HTML in the text it displays (e.g. trace
// names, category labels): links, styled spans, line breaks. Some of
// the charts' text comes from the pictures' metadata (camera and lens
// names), which is untrusted, so it is escaped before plotting; Plotly
// turns the entities back into the characters, so they show as typed.

/** Escapes the characters Plotly reads as markup in its text. */
export function escapePlotText(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

/** The trace fields whose (string) values Plotly displays as text. */
const textFields = ['name', 'text', 'labels', 'x', 'y', 'hovertext'];

function escapeValue(value: unknown): unknown {
  if (typeof value === 'string') {
    return escapePlotText(value);
  }
  if (Array.isArray(value)) {
    return value.map((v: unknown) =>
      typeof v === 'string' ? escapePlotText(v) : v,
    );
  }
  return value;
}

/**
 * Returns copies of the given traces, with the text that Plotly
 * displays from them escaped (see {@link escapePlotText}); numbers and the
 * other fields are kept as they are.
 */
export function escapeTraces<T extends Record<string, unknown>>(
  traces: T[],
): T[] {
  return traces.map((trace) => {
    const escaped: Record<string, unknown> = { ...trace };
    for (const field of textFields) {
      if (field in escaped) {
        escaped[field] = escapeValue(escaped[field]);
      }
    }
    return escaped as T;
  });
}
