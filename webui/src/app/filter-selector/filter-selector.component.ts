import { Component, EventEmitter, Input, Output, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { storeFilterItem } from '../utils/dataStorage'

export type Option = {
    id: number,
    name: string,
    _message_count: number
    _ui_selected_message_count?: number,
};

export type FilterEvent = { option: Option; checked: boolean; id: number;};

@Component({
    selector: 'filter-selector',
    templateUrl: './filter-selector.component.html',
    styleUrls: [ 'filter-selector.component.scss' ]
})
export class FilterSelectorComponent {
    @Input() public title: string;
    @Input() public options: Option[];
    @Output() public toggle = new EventEmitter<FilterEvent>();

    constructor( @Inject(DOCUMENT) private document: Document) {}

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public clickOption(opt, isChecked, idx){
        storeFilterItem(opt.name, !isChecked);
        this.toggle.emit({ option: opt, checked: isChecked, id: idx });
    }

    public selectAll(options) {
        options.forEach(function(opt, idx){
            this.clickOption(opt, true, idx);
        }.bind(this));
    }
    public unselectAll(options) {
        options.forEach(function(opt, idx){
            this.clickOption(opt, false, idx);
        }.bind(this));
    }

    public trackOption(index, option) {
        return option ? option.id : undefined;
    }
}
