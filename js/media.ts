// Helpers for playing media (movies) in the viewer.

/**
 * Returns the message to show the user when a movie's `play()` call fails,
 * or null if the failure needs no message.
 *
 * An `AbortError` means the play request was interrupted, either by a
 * `pause()` call or by the source changing because the user navigated away.
 * Both are user actions, so there is nothing to report.
 */
export function playFailureMessage(err: unknown): string | null {
  if (err instanceof DOMException) {
    switch (err.name) {
      case 'AbortError':
        return null;
      case 'NotAllowedError':
        return 'The browser did not allow playing the movie.';
      case 'NotSupportedError':
        return 'The browser cannot play this movie (unsupported format or failed load).';
    }
  }
  const detail = err instanceof Error ? err.message : String(err);
  return `Cannot play the movie: ${detail}`;
}
