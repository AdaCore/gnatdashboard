/* Define some constants */

export const TN_PATTERN: string =
    '[0-9a-zA-Z][0-9a-cA-C][0-9]{2}-[0123479ACG][0-9]{2}';
export const TN: RegExp = new RegExp('^' + TN_PATTERN + '$', '');
