import { Component, EventEmitter, Input, Output } from '@angular/core';

export type Option = {
    id: number,
    name: string,
    _message_count: number
    _ui_selected_message_count?: number,
};

export type FilterEvent = { option: Option; checked: boolean };

@Component({
    selector: 'filter-selector',
    templateUrl: './filter-selector.component.html',
    styleUrls: [ 'filter-selector.component.scss' ]
})
export class FilterSelectorComponent {
    @Input() public title: string;
    @Input() public options: Option[];
    @Output() public toggle = new EventEmitter<FilterEvent>();
}
