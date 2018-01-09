/*
 * Manage if sorting is ascending or descending.
 * Then call sortBy() to do the work.
 */
export function sortMapInArray(newFilter, filter, map) {
    let array = [];
    if ((filter.newSort === newFilter.newSort
         || filter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = filter.order * -1;
        array =  sortBy(newFilter, filter, map);
    } else {
        if (!newFilter.order) { newFilter.order = 1; }
        array = sortBy(newFilter, filter, map);
    }
    return array;
}

/*
 * A basic sorting function.
 * Take two property names for sorting. If the first doesn't exist, sort by the second.
 */
export function sortBy(newFilter, oldFilter, map) {
    let array = [];
    array = mapToArray(map);
    let property = newFilter.newSort;
    if (array[0][property] == null) {
        property = newFilter.otherSort;
    }

    array.sort((a: any, b: any) => {
        if (a[property] < b[property]) { return -1 * newFilter.order; }
        if (a[property] > b[property]) { return 1 * newFilter.order; }
        return 0;
    });
    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return array;
}

/*
 * Transform the map of the JSON to an array, for easier sorting
 */
function mapToArray(map: any) {
    let array = [];
    Object.keys(map).forEach(name => {
        array.push(map[name]);
    });
    return array;
}
