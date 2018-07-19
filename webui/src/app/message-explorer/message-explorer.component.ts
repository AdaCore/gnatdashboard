import { Component, OnInit, Inject } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { sortMessageArray } from '../utils/sortArray';

import { DOCUMENT } from '@angular/platform-browser';

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

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public expandCollapseAll(badClass: string) {

        if (this.reportService.checkArray(this.reportService.message.sources,
                                          "message-explorer.component",
                                          "expandCollapseAll",
                                          "reportService.message.sources")) {
            this.reportService.message.sources.forEach(function(source){
                let elem = this.document.getElementById(source.filename);
                if (elem) {
                    let idx = elem.classList.contains(badClass);
                    if (idx) {
                        this.openClose(source.filename);
                    }
                }
            }.bind(this));
        }
    }

    public showFilesChanges() {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

}
