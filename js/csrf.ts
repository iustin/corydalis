// Support for the server's CSRF protection: the token is embedded in the
// page, and must be sent back as a header on state-changing requests.

export const CSRF_HEADER = 'X-XSRF-TOKEN';

const SAFE_METHODS = ['GET', 'HEAD', 'OPTIONS', 'TRACE'];

/** Returns the CSRF token from the page's csrf-token meta tag, if any. */
export function csrfTokenFromPage(
  root: Pick<ParentNode, 'querySelector'> = document,
): string | null {
  return (
    root.querySelector<HTMLMetaElement>('meta[name="csrf-token"]')?.content ??
    null
  );
}

/**
 * Whether a failed request was rejected by the CSRF check, which means the
 * page's token is stale. The server returns 403 only for this; requests
 * without a valid session get a 401 instead.
 */
export function isCsrfRejection(status: number): boolean {
  return status === 403;
}

/** Makes jQuery send the token on all same-origin, state-changing requests. */
export function setupCsrf(
  jquery: Pick<JQueryStatic, 'ajaxPrefilter'>,
  getToken: () => string | null = () => csrfTokenFromPage(),
) {
  jquery.ajaxPrefilter((options, _originalOptions, xhr) => {
    const method = (options.type ?? 'GET').toUpperCase();
    if (options.crossDomain || SAFE_METHODS.includes(method)) {
      return;
    }
    const token = getToken();
    if (token !== null) {
      xhr.setRequestHeader(CSRF_HEADER, token);
    }
  });
}
