interface Array<T> {
  sum: (exp?: (item: any) => number) => number;
}

Array.prototype.sum = function(exp?: (item: any) => number): number {
    return this
        .map(exp || (item => item))
        .reduce((count, val) => count + val, 0 /* initialValue */);
};
