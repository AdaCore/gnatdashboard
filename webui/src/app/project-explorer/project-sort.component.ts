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
    let property = newFilter.newSort;

    //Map of Projects to array of projects
    let project_array = mapToArray(map);

    if (project_array[0][property] == null) {
        property = newFilter.otherSort;
    }
    project_array.sort((a: any, b: any) => {
        if (a[property] < b[property]) { return -1 * newFilter.order; }
        if (a[property] > b[property]) { return 1 * newFilter.order; }
        return 0;
    });

    //Map of folders to array of folders
    project_array.forEach(function(project){
        let folder_array =  mapToArray(project.source_dirs);
        if (folder_array[0][property] == null) {
            property = newFilter.otherSort;
        }
        folder_array.sort((a: any, b: any) => {
            if (a[property] < b[property]) { return -1 * newFilter.order; }
            if (a[property] > b[property]) { return 1 * newFilter.order; }
            return 0;
        });

        //Map of files to array of files
        folder_array.forEach(function(folder){
            let file_array = mapToArray(folder.sources);
            folder.name = folder.path.replace(project._source_dirs_common_prefix, '');
            if (file_array[0][property] == null) {
                property = newFilter.otherSort;
            }
            file_array.sort((a: any, b: any) => {
                if (a[property] < b[property]) { return -1 * newFilter.order; }
                if (a[property] > b[property]) { return 1 * newFilter.order; }
                return 0;
            });
            folder.showed_sources = file_array;
        });

        project.showed_dirs = folder_array;
    });

    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return project_array;
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
