var filterName = "filter-";
var messageSortName = "messageExplorer-sort";
var projectSortName = "projectExplorer-sort";

export function getFilterItem(item): Boolean {
    let itemName = filterName + item.name;
    let itemValue = localStorage.getItem(itemName);
    return JSON.parse(itemValue);
}

function getStoredFilterItems(items) {
    items.forEach(function(item){
        let itemName = filterName + item.name;
        let itemValue = localStorage.getItem(itemName);
        item._ui_unselected = JSON.parse(itemValue);
    });
}

export function getStoredFilter(filter) {
    if (filter.tools)
        getStoredFilterItems(filter.tools);
    if (filter.rules)
        getStoredFilterItems(filter.rules);
    if (filter.properties)
        getStoredFilterItems(filter.properties);
    if (filter.ranking)
        getStoredFilterItems(filter.ranking);
    if (filter.review_status)
        getStoredFilterItems(filter.review_status);
}

export function storeFilterItem(name, itemState) {
    let itemName = filterName + name;
    if (itemState){
        localStorage.setItem(itemName, itemState);
    } else {
        localStorage.removeItem(itemName);
    }

}

export function storeMessageSort(value) {
    let itemValue = JSON.stringify(value);
    localStorage.setItem(messageSortName, itemValue);
}

export function getStoredMessageSort(): Object {
    let itemValue = localStorage.getItem(messageSortName);
    if (itemValue != null){
        return JSON.parse(itemValue);
    } else {
        return {newSort: 'ranking', otherSort: 'countRanking', order: -1};
    }
}

export function storeProjectSort(value) {
    let itemValue = JSON.stringify(value);
    localStorage.setItem(projectSortName, itemValue);
}

export function getStoredProjectSort(): Object {
    let itemValue = localStorage.getItem(projectSortName);
    if (itemValue != null){
        return JSON.parse(itemValue);
    } else {
        return {newSort: 'name', otherSort: 'filename', order: 1};
    }
}
