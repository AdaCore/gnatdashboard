/* Define some constants */

export const CODE_REVIEW_URL: string = 'https://git.adacore.com';
export const CHANGE_ID_PATTERN: string = 'I[0-9a-f]{8,40}';
export const CHANGE_ID: RegExp = new RegExp('^' + CHANGE_ID_PATTERN + '$', '');
