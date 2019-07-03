import { Component, OnInit, Inject } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { sortMessageArray } from '../utils/sortArray';

import { DOCUMENT } from '@angular/common';

@Component({
    selector: 'message-explorer',
    templateUrl: './message-explorer.component.html',
    styleUrls: [ 'message-explorer.component.scss' ]
})
export class MessageExplorerComponent implements OnInit {

    constructor(public reportService: SharedReport,
                 @Inject(DOCUMENT) private document: Document) {}
    /** @override */
    public ngOnInit() {
        this.reportService.page = 'message-explorer';
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.message.sources = sortMessageArray(
            {newSort: firstSort, otherSort: secondSort},
            this.reportService.messageFilter,
            this.reportService.message.sources);
    }

    public expandCollapseAll(myValue:boolean) {
        if (this.reportService.checkArray(this.reportService.message.sources,
                                          "message-explorer.component",
                                          "expandCollapseAll",
                                          "reportService.message.sources")) {
            this.reportService.message.sources.forEach(function(source){
                source.expand = myValue;
            }.bind(this));
        }
    }

    public openClose(source) {
        source.expand = !source.expand;
    }

    public showFilesChanges() {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

    public trackMsg(index, message){
        return message ? message.id: undefined;
    }

    public trackSrc(index, source){
        return source ? source.filename: undefined;
    }

}
