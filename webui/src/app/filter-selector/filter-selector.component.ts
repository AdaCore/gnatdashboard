import { Component, EventEmitter, Input, Output, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { storeFilterItem } from '../utils/dataStorage';

export type Option = {
    id: number,
    name: string,
    _message_count: number
    _ui_selected_message_count?: number,
};

export type FilterEvent = { option: Option; checked: boolean; id: number; };

@Component({
    selector: 'filter-selector',
    templateUrl: './filter-selector.component.html',
    styleUrls: [ 'filter-selector.component.scss' ]
})
export class FilterSelectorComponent {
    @Input() public title: string;
    @Input() public options: Option[];
    @Output() public toggle: any = new EventEmitter<FilterEvent>();

    constructor( @Inject(DOCUMENT) private document: Document) {}

    public openClose(id: string): void {
        let elem: HTMLElement = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public clickOption(opt: Option, isChecked: boolean, idx: number): void {
        storeFilterItem(opt.name, !isChecked);
        this.toggle.emit({ option: opt, checked: isChecked, id: idx });
    }

    public selectAll(options: Option[]): void {
        options.forEach(function(opt: Option, idx: number): void {
            this.clickOption(opt, true, idx);
        }.bind(this));
    }

    public unselectAll(options: Option[]): void {
        options.forEach(function(opt: Option, idx: number): void {
            this.clickOption(opt, false, idx);
        }.bind(this));
    }

    public trackOption(index: number, option: Option): number {
        return option ? option.id : undefined;
    }
}
