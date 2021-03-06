import { Component, OnInit, Inject, AfterViewInit } from '@angular/core';
import { SharedReport } from '../main-responder.service';
import { sortMessageArray } from '../utils/sortArray';
import { storeMessageSort } from '../utils/dataStorage';
import { DOCUMENT } from '@angular/common';
import { ISort, ISourceNav, IMessage } from 'gnat';

@Component({
    selector: 'message-explorer',
    templateUrl: './message-explorer.component.html',
    styleUrls: [ 'message-explorer.component.scss' ]
})
export class MessageExplorerComponent implements OnInit, AfterViewInit {

    public sourceMessageList: ISourceNav[] = [];

    constructor(public reportService: SharedReport,
                @Inject(DOCUMENT) private document: Document) {}
    /** @override */
    public ngOnInit(): void {
        localStorage.setItem('defaultMain', '/message-explorer');
    }
    public ngAfterViewInit(): void {
        setTimeout(() => {
            this.reportService.setPage('message-explorer');
        });
    }
    public toList(sources: any): ISourceNav[] {
        if (this.sourceMessageList.length === 0) {
            this.sourceMessageList = Object['values'](sources);
        }
        return this.sourceMessageList;
    }

    public sortModules(firstSort: string, secondSort: string): void {
        let newFilter: ISort = {newSort: firstSort, otherSort: secondSort};
        this.reportService.message.sources = sortMessageArray(
            newFilter,
            this.reportService.messageSort,
            this.toList(this.reportService.message.sources));
        storeMessageSort(newFilter);
    }

    public expandCollapseAll(myValue: boolean): void {
        if (this.reportService.checkArray(
            this.toList(this.reportService.message.sources),
            'message-explorer.component',
            'expandCollapseAll',
            'reportService.message.sources')) {
            this.toList(this.reportService.message.sources).forEach(
                function(source: ISourceNav): void{
                    this.openClose(source, myValue);
                }.bind(this));
        }
    }

    public openClose(source: ISourceNav, isOpen: boolean): void {
        source.expand = isOpen;
    }

    public showFilesChanges(): void {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

    public trackMsg(index: number, message: IMessage): number{
        return message ? message.id : undefined;
    }

    public trackSrc(index: number, source: ISourceNav): string{
        return source ? source.filename : undefined;
    }
}
