/*
 *  Sort the array containing all the information
 *  for code navigation according to the given filter
 */
export function sortCodeArray(newFilter, oldFilter, project_array) {
    if ((oldFilter.newSort === newFilter.newSort
         || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = oldFilter.order * -1;
    } else {
        newFilter.order = 1;
    }

    let property = project_array[0][newFilter.newSort] != null ?
        newFilter.newSort : (project_array[0][newFilter.otherSort] != null ? newFilter.otherSort : null);

    if (property != null) {
        project_array.sort((a: any, b: any) => {
            if (a[property] < b[property]) { return -1 * newFilter.order; }
            if (a[property] > b[property]) { return 1 * newFilter.order; }
            return 0;
        });
    } else {
        console.log("bad filter", newFilter, project_array)
    }

    //Map of folders to array of folders
    project_array.forEach(function(project){
        let folder_array =  project.source_dirs;

        property = folder_array[0][newFilter.newSort] != null ?
            newFilter.newSort : (folder_array[0][newFilter.otherSort] != null ? newFilter.otherSort : null);

        if (property != null) {
            folder_array.sort((a: any, b: any) => {
                if (a[property] < b[property]) { return -1 * newFilter.order; }
                if (a[property] > b[property]) { return 1 * newFilter.order; }
                return 0;
            });
        } else {
            console.log("bad filter", newFilter, folder_array)
        }

        //Map of files to array of files
        folder_array.forEach(function(folder){
            let file_array = folder.sources;
            folder.name = folder.name.replace(project._source_dirs_common_prefix, '');

            property = file_array[0][newFilter.newSort] != null ?
                newFilter.newSort : (file_array[0][newFilter.otherSort] != null ? newFilter.otherSort : null);
            if (property != null) {
                file_array.sort((a: any, b: any) => {
                    if (a[property] < b[property]) { return -1 * newFilter.order; }
                    if (a[property] > b[property]) { return 1 * newFilter.order; }
                    return 0;
                });
            } else {
                console.log("bad filter", newFilter, file_array)
            }
            folder.sources = file_array;
        });

        project.source_dirs = folder_array;
    });

    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return project_array;
}

/*
 *  Sort the array containing all the information
 *  for message navigation according to the given filter
 */
export function sortMessageArray(newFilter, oldFilter, source_array) {
    if ((oldFilter.newSort === newFilter.newSort
         || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = oldFilter.order * -1;
    } else {
        newFilter.order = 1;
    }

    let property = source_array[0][newFilter.newSort] != null ?
        newFilter.newSort : (source_array[0][newFilter.otherSort] != null ? newFilter.otherSort : null);


    if (property != null) {
        source_array.sort((a: any, b: any) => {
            if (a[property] < b[property]) {
                return -1 * newFilter.order;
            }
            if (a[property] > b[property]) {
                return 1 * newFilter.order;
            }
            return 0;
        });
    } else {
        console.log("bad filter", newFilter, source_array)
    }

    source_array.forEach(function(source){
        if (source.messages){
            let message_array =  source.messages;
            property = message_array[0][newFilter.newSort] != null ?
                newFilter.newSort : (message_array[0][newFilter.otherSort] != null ? newFilter.otherSort : null);

            if (property != null) {
               /* console.log("property", property);*/
                message_array.sort((a: any, b: any) => {
                   /* console.log("a[property]", a[property]);
                    console.log("b[property]", b[property]);*/
                    if (a[property] < b[property]) { return -1 * newFilter.order; }
                    if (a[property] > b[property]) { return 1 * newFilter.order; }
                    return 0;
                });
            } else  {
                console.log("bad filter", newFilter, message_array)
            }
            source.messages = message_array;
        }
    });

    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return source_array;
}
