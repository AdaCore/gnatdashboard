import { Component, Input } from '@angular/core';
import { IAnnotatedSourceMessage, ITool } from 'gnat';
import { SharedReport } from '../main-responder.service'

type InlineMessages = { [toolId: number]: Set<IAnnotatedSourceMessage> };

@Component({
    selector: 'inline-messages',
    templateUrl: 'inline-messages.component.html',
    styleUrls: [ 'inline-messages.component.scss' ]
})
export class InlineMessagesComponent {
    @Input() public messages: InlineMessages[];
    @Input() public tools: { [toolId: number]: ITool };

    constructor( public reportService: SharedReport) {}

    public formatMessageProperties(message: IAnnotatedSourceMessage): string {
        return message.properties.map(prop => prop.name).join(', ');
    }

    public getToolName(id: number) {
        let toolName: string = "";
        this.reportService.filter.tools.forEach(function(tool){
            if(tool.id == id){
                toolName = tool.name;
            }
        });
        return toolName;
    }
}
