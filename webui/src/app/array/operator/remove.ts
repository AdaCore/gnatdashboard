interface Array<T> {
  remove: (from: number, to?: number) => number;
}

/**
 * Delete element(s) between |from| and |to|.
 *
 * @param from The index from where to start deleting elements.
 * @param to The last index to delete. If omitted, delete only the element at
 *      index |from|.
 * @returns The new length of the array.
 */
Array.prototype.remove = (from: number, to?: number): number => {
    let rest = this.slice((to || from) + 1 || this.length);
    this.length = from < 0 ? this.length + from : from;
    return this.push.apply(this, rest);
};
