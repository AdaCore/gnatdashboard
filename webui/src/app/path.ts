/**
 * Compute the longest common prefix in |paths|.
 *
 * @param paths An array of string from which to extract the longest common
 *      prefix.
 * @return The longest common prefix if any, the empty string otherwise.
 */
export function commonprefix(paths: string[]): string {
    'use strict';

    // Take care of the null case.
    if (!paths || paths.length === 0) {
        return '';
    }

    // |concat()| creates a copy of the array and returns it, and |sort()| works
    // in place (on the copy) and returns the input array.
    let A: string[] = paths.concat().sort();

    // Compare the first and last string now that the array is sorted.
    let i: number = 0;
    let a1: string = A[0];
    let a2: string = A[A.length - 1];
    for (; i < a1.length && a1.charAt(i) === a2.charAt(i); i++) {
        // Loop until exit
    }
    return a1.substring(0, i);
}
