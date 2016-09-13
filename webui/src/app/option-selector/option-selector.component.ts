import { Component, Input } from '@angular/core';

@Component({
    selector: 'option-selector',
    templateUrl: './option-selector.template.html',
    styleUrls: [ './option-selector.style.css' ]
})
export class OptionSelector {
    @Input() title: string = null;
    @Input() options: { name: string, ui_selected: boolean }[] = null;
    @Input() optionCount: (
        option: { name: string, ui_selected: boolean }) => number = null;
}
