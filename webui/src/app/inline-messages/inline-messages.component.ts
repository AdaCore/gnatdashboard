import { Component, Input } from '@angular/core';
import { IGNAThubMessage } from 'gnat';
import { IGNAThubTool } from 'gnat';

export type InlineMessages = { [toolId: number]: IGNAThubMessage[] };

@Component({
    selector: 'inline-messages',
    templateUrl: 'inline-messages.component.html',
    styleUrls: [ 'inline-messages.component.scss' ]
})
export class InlineMessagesComponent {
    @Input() public messages: InlineMessages = null;
    @Input() public tools: { [toolId: number]: IGNAThubTool } = null;

    public formatMessageProperties(message: IGNAThubMessage): string {
        return message.properties.map(prop => prop.name).join(', ');
    }
}
