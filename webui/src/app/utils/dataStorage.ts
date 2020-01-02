import { IFilterIndex, ISort } from 'gnat';

const filterName: string = 'filter-';
const messageSortName: string = 'messageExplorer-sort';
const projectSortName: string = 'projectExplorer-sort';

export function getFilterItem(item: any): boolean {
    let itemName: string = filterName + item.name;
    let itemValue: string = localStorage.getItem(itemName);
    return JSON.parse(itemValue);
}

function getStoredFilterItems(items: [any]): void {
    items.forEach(function(item: any): void {
        let itemName: string = filterName + item.name;
        let itemValue: string = localStorage.getItem(itemName);
        item._ui_unselected = JSON.parse(itemValue);
    });
}

export function getStoredFilter(filter: IFilterIndex): void {
    if (filter.tools){
        getStoredFilterItems(filter.tools);
    }
    if (filter.rules){
        getStoredFilterItems(filter.rules);
    }
    if (filter.properties){
        getStoredFilterItems(filter.properties);
    }
    if (filter.ranking){
        getStoredFilterItems(filter.ranking);
    }
    if (filter.review_status){
        getStoredFilterItems(filter.review_status);
    }
}

export function storeFilterItem(name: string, itemState: boolean): void {
    let itemName: string = filterName + name;
    if (itemState){
        localStorage.setItem(itemName, itemState.toString());
    } else {
        localStorage.removeItem(itemName);
    }

}

export function storeMessageSort(value: ISort): void {
    let itemValue: string  = JSON.stringify(value);
    localStorage.setItem(messageSortName, itemValue);
}

export function getStoredMessageSort(): ISort {
    let itemValue: string  = localStorage.getItem(messageSortName);
    if (itemValue != null){
        return JSON.parse(itemValue);
    } else {
        return {newSort: 'ranking', otherSort: 'countRanking', order: -1};
    }
}

export function storeProjectSort(value: ISort): void {
    let itemValue: string  = JSON.stringify(value);
    localStorage.setItem(projectSortName, itemValue);
}

export function getStoredProjectSort(): ISort {
    let itemValue: string  = localStorage.getItem(projectSortName);
    if (itemValue != null){
        return JSON.parse(itemValue);
    } else {
        return {newSort: 'name', otherSort: 'filename', order: 1};
    }
}
