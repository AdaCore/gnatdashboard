import { Component, Input } from '@angular/core';
import { IAnnotatedSourceMessage, ITool } from 'gnat';
import { SharedReport } from '../main-responder.service';

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

    public getToolName(id: number): string {
        let toolName: string = '';

        if (this.reportService.filter) {
            if (this.reportService.checkArray(this.reportService.filter.tools,
                                              'inline-messages.component',
                                              'getToolName', 'reportService.filter.tools')) {
                this.reportService.filter.tools.forEach(function(tool: ITool): void {
                    if (tool.id === id){
                        toolName = tool.name;
                    }
                });
            }
        } else {
            console.log('[Error] inline-messages.component:getToolName : '
                        + "reportService.filter doesn't exist.");
        }
        return toolName;
    }
}
