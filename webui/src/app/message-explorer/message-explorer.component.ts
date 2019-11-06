import { Component, OnInit, Inject } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { sortMessageArray } from '../utils/sortArray';
import { storeMessageSort } from '../utils/dataStorage'
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
        localStorage.setItem('defaultMain', '/message-explorer');
    }

    public sortModules(firstSort: string, secondSort: string) {
        let newFilter = {newSort: firstSort, otherSort: secondSort};
        this.reportService.message.sources = sortMessageArray(
            newFilter,
            this.reportService.messageSort,
            this.reportService.message.sources);
        storeMessageSort(newFilter);
    }

    public expandCollapseAll(myValue:boolean) {
        if (this.reportService.checkArray(this.reportService.message.sources,
                                          "message-explorer.component",
                                          "expandCollapseAll",
                                          "reportService.message.sources")) {
            this.reportService.message.sources.forEach(function(source){
                this.openClose(source, myValue);
            }.bind(this));
        }
    }

    public openClose(source, isOpen) {
        source.expand = isOpen;
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
