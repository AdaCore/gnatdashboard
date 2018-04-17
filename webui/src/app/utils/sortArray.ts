/*
 *  Sort the array containing all the information
 *  for code navigation according to the given filter
 */
export function sortCodeArray(newFilter, oldFilter, projectArray) {
    if ((oldFilter.newSort === newFilter.newSort
         || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = oldFilter.order * -1;
    } else if (newFilter.order) {
        newFilter.order = newFilter.order;
    } else {
        newFilter.order = 1;
    }
    let property = projectArray[0][newFilter.newSort] != null ?
        newFilter.newSort : (projectArray[0][newFilter.otherSort] != null ?
                             newFilter.otherSort : null);

    if (property != null) {
        projectArray.sort((a: any, b: any) => {
            if (a[property] < b[property]) { return -1 * newFilter.order; }
            if (a[property] > b[property]) { return 1 * newFilter.order; }
            return 0;
        });
    } else {
        console.log('bad filter', newFilter, projectArray);
    }

    // Map of folders to array of folders
    projectArray.forEach(function(project){
        let folderArray =  project.source_dirs;

        property = folderArray[0][newFilter.newSort] != null ?
            newFilter.newSort : (folderArray[0][newFilter.otherSort] != null ?
                                 newFilter.otherSort : null);

        if (property != null) {
            folderArray.sort((a: any, b: any) => {
                if (a[property] < b[property]) { return -1 * newFilter.order; }
                if (a[property] > b[property]) { return 1 * newFilter.order; }
                return 0;
            });
        } else {
            console.log('bad filter', newFilter, folderArray);
        }

        // Map of files to array of files
        folderArray.forEach(function(folder){
            let fileArray = folder.sources;
            folder.name = folder.name.replace(project._source_dirs_common_prefix, '');

            property = fileArray[0][newFilter.newSort] != null ?
                newFilter.newSort : (fileArray[0][newFilter.otherSort] != null ?
                                     newFilter.otherSort : null);
            if (property != null) {
                fileArray.sort((a: any, b: any) => {
                    if (a[property] < b[property]) { return -1 * newFilter.order; }
                    if (a[property] > b[property]) { return 1 * newFilter.order; }
                    return 0;
                });
            } else {
                console.log('bad filter', newFilter, fileArray);
            }
            folder.sources = fileArray;
        });

        project.source_dirs = folderArray;
    });

    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return projectArray;
}

/*
 *  Sort the array containing all the information
 *  for message navigation according to the given filter
 */
export function sortMessageArray(newFilter, oldFilter, sourceArray) {
    if ((oldFilter.newSort === newFilter.newSort
         || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = oldFilter.order * -1;
    } else if (newFilter.order) {
        newFilter.order = newFilter.order;
    } else {
        newFilter.order = 1;
    }

    let property = sourceArray[0][newFilter.newSort] != null ?
        newFilter.newSort : (sourceArray[0][newFilter.otherSort] != null ?
                             newFilter.otherSort : null);


    if (property != null) {
        sourceArray.sort((a: any, b: any) => {
            if (a[property] < b[property]) {
                return -1 * newFilter.order;
            }
            if (a[property] > b[property]) {
                return 1 * newFilter.order;
            }
            return 0;
        });
    } else if (newFilter.otherSort != '') {
        console.log('bad filter', newFilter, sourceArray);
    }

    sourceArray.forEach(function(source){
        if (source.messages){
            let messageArray =  source.messages;
            property = messageArray[0][newFilter.newSort] != null ?
                newFilter.newSort : (messageArray[0][newFilter.otherSort] != null ?
                                     newFilter.otherSort : null);

            if (property == "ranking") {
                messageArray.sort((a: any, b: any) => {
                    if (a['ranking']['id'] < b['ranking']['id']) { return -1 * newFilter.order; }
                    if (a['ranking']['id'] > b['ranking']['id']) { return 1 * newFilter.order; }
                    return 0;
                });
            } else if (property != null) {
                messageArray.sort((a: any, b: any) => {
                    if (a[property] < b[property]) { return -1 * newFilter.order; }
                    if (a[property] > b[property]) { return 1 * newFilter.order; }
                    return 0;
                });
            }
            source.messages = messageArray;
        }
    });

    oldFilter.order = newFilter.order;
    oldFilter.newSort = newFilter.newSort;
    oldFilter.otherSort = newFilter.otherSort;
    return sourceArray;
}
