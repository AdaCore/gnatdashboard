import {IMessage, IModule, ISort, ISource, ISourceDir, ISourceNav} from 'gnat';

/*
 *  Sort the array containing all the information
 *  for code navigation according to the given filter
 */
export function sortCodeArray(newFilter: ISort, oldFilter: ISort,
                              projectArray: IModule[]): IModule[] {
    if ((oldFilter.newSort === newFilter.newSort
         || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
        newFilter.order = oldFilter.order * -1;
    } else if (newFilter.order) {
        newFilter.order = newFilter.order;
    } else {
        newFilter.order = 1;
    }

    let property: string = checkProperty(projectArray, newFilter);

    if (property != null) {
        projectArray.sort((a: any, b: any) => {
            if (a[property] < b[property]) { return -1 * newFilter.order; }
            if (a[property] > b[property]) { return 1 * newFilter.order; }
            return 0;
        });
    } else {
        console.log('bad filter', newFilter);
    }

    // Map of folders to array of folders
    projectArray.forEach(function(project: IModule): void {
        let folderArray: ISourceDir[] =  project.source_dirs;

        property = checkProperty(folderArray, newFilter);

        if (property != null) {
            folderArray.sort((a: any, b: any) => {
                if (a[property] < b[property]) { return -1 * newFilter.order; }
                if (a[property] > b[property]) { return 1 * newFilter.order; }
                return 0;
            });
        } else {
            console.log('bad filter', newFilter);
        }

        // Map of files to array of files
        folderArray.forEach(function(folder: ISourceDir): void {
            let fileArray: ISource[] = folder.sources;
            folder.name =
                (folder.name !== project._source_dirs_common_prefix ?
                 folder.name.replace(project._source_dirs_common_prefix, '')
                 : '.');

            property = checkProperty(fileArray, newFilter);

            if (property != null) {
                fileArray.sort((a: any, b: any) => {
                    if (a[property] < b[property]) { return -1 * newFilter.order; }
                    if (a[property] > b[property]) { return 1 * newFilter.order; }
                    return 0;
                });
            } else {
                console.log('bad filter', newFilter);
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

function checkProperty(myArray: any, newFilter: ISort): string {
  return myArray[0][newFilter.newSort] != null ?
      newFilter.newSort : (myArray[0][newFilter.otherSort] != null ?
        newFilter.otherSort : null);
}

function sortRanking(a: number, b: number): number{
    let tmp: number = 0;
    if (a < b){
        tmp = -1;
    } else if (a > b){
        tmp = 1;
    }
    return tmp;
}

/*
 *  Sort the array containing all the information
 *  for message navigation according to the given filter
 */
export function sortMessageArray(
    newFilter: ISort, oldFilter: ISort,
    sourceArray: ISourceNav[]): ISourceNav[] {
        if ((oldFilter.newSort === newFilter.newSort
             || oldFilter.otherSort === newFilter.otherSort) && !newFilter.order) {
            newFilter.order = oldFilter.order * -1;
        } else if (newFilter.order) {
            newFilter.order = newFilter.order;
        } else {
            newFilter.order = 1;
        }
        let property: string = checkProperty(sourceArray, newFilter);

        if (property === 'countRanking'){
            sourceArray.sort((a: ISourceNav, b: ISourceNav) => {
                let tmp: number = sortRanking(a.countRanking.High, b.countRanking.High);
                if (tmp === 0){
                    tmp = sortRanking(a.countRanking.Medium, b.countRanking.Medium);
                    if (tmp === 0){
                        tmp = sortRanking(a.countRanking.Low, b.countRanking.Low);
                        if (tmp === 0){
                            tmp = sortRanking(a.countRanking.Info, b.countRanking.Info);
                            if (tmp === 0){
                                tmp = sortRanking(a.countRanking.Unspecified,
                                                  b.countRanking.Unspecified);
                            }
                        }
                    }
                }
                return tmp * newFilter.order;
            });
        } else if (property != null) {
            sourceArray.sort((a: ISourceNav, b: ISourceNav) => {
                if (a[property] < b[property]) {
                    return -1 * newFilter.order;
                }
                if (a[property] > b[property]) {
                    return 1 * newFilter.order;
                }
                return 0;
            });
        } else if (newFilter.otherSort !== '') {
            console.log('bad filter', newFilter);
        }

        sourceArray.forEach(function(source: ISourceNav): void {
            if (source.messages && newFilter.newSort !== 'status_priority'){
                let messageArray: IMessage[] =  source.messages;
                property = checkProperty(messageArray, newFilter);
                if (property === 'ranking') {
                    messageArray.sort((a: IMessage, b: IMessage) => {
                        if (a['ranking']['id'] < b['ranking']['id']) {
                            return -1 * newFilter.order;
                        }
                        if (a['ranking']['id'] > b['ranking']['id']) {
                            return 1 * newFilter.order;
                        }
                        return 0;
                    });
                } else if (property != null) {
                    messageArray.sort((a: IMessage, b: IMessage) => {
                        if (a[property] < b[property]) {
                            return -1 * newFilter.order;
                        }
                        if (a[property] > b[property]) {
                            return 1 * newFilter.order;
                        }
                        return 0;
                    });
                }
                source.messages = messageArray;
            } else if (source.messages
                       && newFilter.newSort === 'status_priority') {
                let messageArray: IMessage[] =  source.messages;
                messageArray.sort((a: any, b: any) => {
                    let typeA: number = a['status_priority'] ?
                        a['status_priority'] : 0;
                    let typeB: number = b['status_priority'] ?
                        b['status_priority'] : 0;
                    if (typeA < typeB) {
                        return -1 * newFilter.order;
                    }
                    if (typeA > typeB) {
                        return 1 * newFilter.order;
                    }
                    return 0;
                });
                source.messages = messageArray;
            }
        });

        oldFilter.order = newFilter.order;
        oldFilter.newSort = newFilter.newSort;
        oldFilter.otherSort = newFilter.otherSort;
        return sourceArray;
    };
