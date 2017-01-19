import { Component, Input } from '@angular/core';

@Component({
    selector: 'option-selector',
    templateUrl: './option-selector.component.html',
    styleUrls: [ 'option-selector.component.scss' ]
})
export class OptionSelectorComponent {
    @Input() public title: string = null;
    @Input() public options: Array<{ name: string, ui_selected: boolean }> = null;
    @Input() public optionCount: (
        option: { name: string, ui_selected: boolean }) => number = null;
}
