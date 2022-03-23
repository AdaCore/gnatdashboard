import {
    AfterViewInit,
    Component,
    ElementRef,
    HostBinding,
    Inject,
    Input,
    OnDestroy,
    OnInit,
    ViewChild,
    ChangeDetectorRef,
    ChangeDetectionStrategy
} from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { ActivatedRoute } from '@angular/router';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';

import { SharedReport } from '../main-responder.service';
import { DialogsService } from './dialog.service';
import { GNAThubService } from '../gnathub.service';

import { AnnotatedSourceViewComponent } from './annotated-source-view.component';

import { updateFilter } from '../utils/refreshFilter';
import { sortMessageArray } from '../utils/sortArray';
import { storeMessageSort } from '../utils/dataStorage';

import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
    IMessage,
    IReviewFilter,
    ISourceNav,
    IReviewUser,
    ISort
} from 'gnat';
import {LoadJsonService} from "../load-json.service";

type MessagesByToolId = { [toolId: number]: IAnnotatedSourceMessage[] };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ],
    providers: [DialogsService]
})
export class AnnotatedSourceComponent
    implements AfterViewInit, OnDestroy, OnInit
{
    @Input() public source: IAnnotatedSourceFile;
    public id: number = -1;
    public displayMessages: boolean = true;
    @HostBinding('class.with-coverage') public displayCoverage: boolean = true;
    public showMessageList: boolean = true;
    public inlineAnnotations: { [line: number]: IAnnotatedSourceMessage[] };
    public tabOpen: string = 'message';
    public checkedMsg: number[] = [];
    public selectedMsg: number[] = [];
    private lastSelectedMsg: number = -1;
    private sub: Subscription;
    public selectedLine: number;
    public selectedId: number;
    public raceEntry: any = [];
    public raceSort: any = {};
    public raceSelected: string = '';
    public isLoadingFile: boolean = false;
    public sourceMessageList: ISourceNav[] = [];

    @ViewChild('scrollView', {static: true}) private scrollView: ElementRef;
    thisFile: boolean

    constructor( @Inject(DOCUMENT) private document: Document,
                 private route: ActivatedRoute,
                 private router: Router,
                 public reportService: SharedReport,
                 public dialog: DialogsService,
                 private sourceView: AnnotatedSourceViewComponent,
                 private gnathub: GNAThubService,
                 private cdRef: ChangeDetectorRef,
                 private loadJSONService: LoadJsonService) {}

    /** @override */
    public ngOnInit(): void {
        this.sub = this.route.params.subscribe(params => {
            let filename: string = params['filename'];
            let line: number = params['line'];
            let id: number = params['id'];

            this.selectedLine = line;
            this.selectedId = id;
            if (filename && filename !== this.sourceView.filename){
                this.reloadFile(filename, line, id);
            } else {
                this.processSource();
            }
            setTimeout(() => {
                this.reportService.setPage(filename);
            });
        });
    }

    private reloadFile(filename: string, line: number, id: number): void {
        const url: string = 'source/' +  filename + '.js';
        this.isLoadingFile = true;
        this.loadJSONService.getJSON(url).subscribe(
          (data : IAnnotatedSourceFile) => {
            this.isLoadingFile = false;
            this.source = data;
            this.sourceView.filename = filename;
            this.processSource();
        }, error => {
            this.isLoadingFile = false;
            console.error('[Error] reloadFile :', error);
        }
      );
    }

    private processSource(): void {
        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || [];
        this.createInlineAnnotations();
        this.afterInitProcess(this.selectedLine, this.selectedId);
        this.checkChanges();
    }

    private afterInitProcess(line: number, id: number): void {
        if (line){
            this.goToLine(line);
        }
        if (id){
            this.initSelectMsg(id);
        }
    }

    private createInlineAnnotations(): void {
        // Prepare inline annotations.
        this.inlineAnnotations = {};
        this.source.annotations = this.source.annotations || [];
        if (this.source) {
            if (this.reportService.checkArray(
                this.source.annotations,
                'annotated-source.component',
                'inlineAnnotations', 'source.annotations')) {
                this.reportService.isAnnotations = true;
                this.source.annotations.forEach(function(
                    annotation: IAnnotatedSourceMessage): void {
                        const toolId: number = annotation.rule.tool_id;
                        const line: number = annotation.line;
                        if (!this.inlineAnnotations.hasOwnProperty(line)) {
                            this.inlineAnnotations[line] = new Set();
                        }
                        this.inlineAnnotations[line].add(annotation);
                    }.bind(this));
            } else {
                this.reportService.isAnnotations = false;
            }
        } else {
            console.log('[Error] annotated-source.component:inlineAnnotations :'
                        + " source doesn't exist.");
        }
    }

    /** @override */
    public ngAfterViewInit(): void {
        switch (document.readyState) {
            case 'interactive':
                console.log('[Warning] annotated-source : afterViewInit :'
                            + ' Document still loading : scrolling can fail');
                this.afterInitProcess(this.selectedLine, this.selectedId);
                break;
            case 'complete':
                this.afterInitProcess(this.selectedLine, this.selectedId);
                break;
            default:
                this.afterInitProcess(this.selectedLine, this.selectedId);
                break;
        }
        this.cdRef.detectChanges();
    }

    /** @override */
    public ngOnDestroy(): void {
        if (this.sub) {
            this.sub.unsubscribe();
        }
    }

    public toList(sources: any): ISourceNav[] {
        if (this.sourceMessageList.length === 0) {
            this.sourceMessageList = Object['values'](sources);
        }
        return this.sourceMessageList;
    }

    public checkChanges(): void {
        this.cdRef.markForCheck();
    }
    private initSelectMsg(id: number): void {
        if (this.reportService.message){
            if (id !== -1) {
                let tmp: any = this.reportService.message;
                if (tmp.sources[this.source.filename]) {
                    let source: ISourceNav = tmp.sources[this.source.filename];
                    let index: number = this.getIndex(id, source.messages);
                    this.selectMessage(source.messages[index], null);
                }
            }
        } else  {
            console.log('[Error] annotated-source.component:initSelectMsg :'
                        + " reportService.message doesn't exist.");
        }
        this.checkChanges();
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    public goToLine(line: number): void {
        if (line) {
            this.selectedLine = line;
            let id: string = 'L' + line;
            try {
                let elem: HTMLElement = this.document.getElementById(id);
                elem.scrollIntoView({block: 'center', inline: 'nearest'});
            } catch (err) {
                console.warn(err);
            }
        }
    };

    public openClose(id: string): void {
        let elem: HTMLElement = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public changeFile(filename: string, fileLine: number, myId: number): void {
        this.router.navigate(['/source', filename, { line: fileLine, id: myId }]);
    }

    public checkMessage(message: any): void {
        if (message){
            let isCodepeerMsg: boolean = (this.reportService.codepeerCode ===
                                          message.rule.tool_id ? true : false);
            if (isCodepeerMsg){
                if (this.checkedMsg.length === 0) {
                    this.reportService.selectedMessage = [];
                }
                let index: number = this.checkedMsg.indexOf(message.tool_msg_id);
                if (index !== -1) {
                    this.checkedMsg.splice(index, 1);
                    this.reportService.selectedMessage.splice(index, 1);
                } else {
                    this.checkedMsg.push(message.tool_msg_id);
                    this.reportService.selectedMessage.push(message);
                }
            }
        }
    }

    private getIndex(id: number, array: any[]): number {
        let index: number = -1;
        let id2: string = id + '';
        for (let idx: number = 0; idx < array.length - 1; idx++) {
            let cellId: string = array[idx].id + '';
            if (cellId === id2){
                index = idx;
                break;
            }
        }
        return index;
    }

    private toggleSelectedMsg(id: number): void {
        let tmp: number = this.selectedMsg.indexOf(id);
        if (tmp === -1) {
            this.selectedMsg.push(id);
        } else {
            this.selectedMsg.splice(tmp, 1);
        }
    }

    public selectMessage( message: any, event: MouseEvent): void {
        if (message){
            this.lastSelectedMsg = message.id;
        }
        if (event && event.ctrlKey && message) {
            this.toggleSelectedMsg(message.id);
            this.checkMessage(message);
            this.selectedLine = -1;
        } else {
            this.checkedMsg = [];
            this.selectedMsg = [];
            this.reportService.selectedMessage = [];
            this.checkMessage(message);
            if (message){
                this.goToLine(message.line);
                this.toggleSelectedMsg(message.id);
            }
        }
    }

    public showReviewsChanges(): void {
        this.reportService.showReviews = !this.reportService.showReviews;
    }

    public ShowReviewHistory(message: any): void {
        this.reportService.history.reviews = message.review_history;
        this.reportService.history.message = message;
        this.dialog.reviewHistory().subscribe((data: any) => {
            console.log('Open reviewHistoryDialog');
        });
    }

    private refreshFilter(): void {
        let userReviewFilter: [IReviewFilter];
        if (this.reportService.message.sources) {
            Object['entries'](this.reportService.message.sources).forEach(
                function(val: any): void {
                    let source: ISourceNav = val[1];
                    if (source.messages){
                        source.messages.forEach(function(message: IMessage): void {
                            if (message.user_review){
                                userReviewFilter = this.reportService.putInFilter(
                                    message.user_review,
                                    userReviewFilter);
                            }
                        }.bind(this));
                    }

                }.bind(this));
        }

        let count: number = this.reportService
            .countUncategorized(userReviewFilter,
                                this.reportService.filter
                                ._total_message_count);
        if (count > 0){
            let tmpReview: any = {
                status: 'UNCATEGORIZED',
                display_name: 'Uncategorized'
            };
            this.reportService.putInFilter(tmpReview, userReviewFilter);
            this.reportService.countUncategorized(
                userReviewFilter,
                this.reportService.filter._total_message_count);
        }

        this.reportService.filter.review_status = userReviewFilter;
        updateFilter(this.reportService);
    }


    private formatName(name: string): string {
        let myArray: string[] = name.split('_');
        let displayName: string = '';
        myArray.forEach(function(word: string, idx: number): void {
            displayName += word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
            if (idx < myArray.length - 1){ displayName += ' '; }
        });
        return displayName;
    }

    private formatDate(int: number): string {
        return int < 10 ? '0' + int.toString() : int.toString();
    }

    public trackMsg(index: number, message: any): void {
        return message ? message.id : undefined;
    }

    public trackSrc(index: number, source: any): void{
        return source ? source.filename : undefined;
    }

    public sortModules(firstSort: string, secondSort: string): void {
        let newFilter: ISort = {newSort: firstSort, otherSort: secondSort};
        this.sourceMessageList = sortMessageArray(
            newFilter,
            this.reportService.messageSort,
            this.toList(this.reportService.message.sources));
        storeMessageSort(newFilter);
    }

    private compareValues(key: string, order: number): any {
        return function innerSort(a: any, b: any): number {
            const varA: string = a[key].toUpperCase();
            const varB: string = b[key].toUpperCase();

            let comparison: number = 0;
            if (varA > varB) {
                comparison = 1;
            } else if (varA < varB) {
                comparison = -1;
            }
            return comparison * order;
        };
    }

    private compareLocation(order: number): any {
        return function innerSort(a: any, b: any): number {
            const fileA: string = a.file.toUpperCase();
            const fileB: string = b.file.toUpperCase();

            let comparison: number = 0;
            if (fileA > fileB) {
                comparison = 1;
            } else if (fileA < fileB) {
                comparison = -1;
            } else if (fileA === fileB) {
                if (a.line > b.line) {
                    comparison = 1;
                } else if (a.line < b.line) {
                    comparison = -1;
                }
            }
            return comparison * order;
        };
    }

    public sortRace(attr: string): void {
        let order: number = 1;

        if (this.raceSort.value === attr) {
            order = this.raceSort.order * -1;
        }
        if (attr === 'location'){
            this.raceEntry.sort(this.compareLocation(order));
        } else {
            this.raceEntry.sort(this.compareValues(attr, order));
        }

        this.raceSort.value = attr;
        this.raceSort.order = order;
    }

    public showAnnotationsChanges(): void {
        this.reportService.showAnnotations = !this.reportService.showAnnotations;
    }

}
