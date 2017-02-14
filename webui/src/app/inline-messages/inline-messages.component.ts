import { Component, Input } from '@angular/core';
import { IAnnotatedSourceMessage } from 'gnat';
import { ITool } from 'gnat';

type InlineMessages = { [toolId: number]: Set<IAnnotatedSourceMessage> };

@Component({
    selector: 'inline-messages',
    templateUrl: 'inline-messages.component.html',
    styleUrls: [ 'inline-messages.component.scss' ]
})
export class InlineMessagesComponent {
    @Input() public messages: InlineMessages[];
    @Input() public tools: { [toolId: number]: ITool };

    public formatMessageProperties(message: IAnnotatedSourceMessage): string {
        return message.properties.map(prop => prop.name).join(', ');
    }
}
