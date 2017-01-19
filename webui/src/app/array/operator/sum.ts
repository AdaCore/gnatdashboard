interface Array<T> {
  sum: (exp?: (item: any) => number) => number;
}

/**
 * Sum all elements of the array.
 *
 * @param exp If specified, map array elements with this function; otherwise the
 *      identity function is used.
 * @returns The sum of the elements in the array.
 */
Array.prototype.sum = (exp?: (item: any) => number): number => {
    return this
        .map(exp || (item => item))
        .reduce((count, val) => count + val, 0 /* initialValue */);
};
