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
    ChangeDetectorRef
} from '@angular/core';
import { Http, Response } from '@angular/http';
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
import { ScrollToService, ScrollToConfigOptions } from '@nicky-lenaers/ngx-scroll-to';

import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
    IMessage,
    IReviewFilter,
    ISourceNav,
    IReviewUser,
    ISort
} from 'gnat';

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

    @ViewChild('scrollView', {static: true}) private scrollView: ElementRef;

    constructor( @Inject(DOCUMENT) private document: Document,
                 private scrollToService: ScrollToService,
                 private route: ActivatedRoute,
                 private router: Router,
                 public reportService: SharedReport,
                 public dialog: DialogsService,
                 private sourceView: AnnotatedSourceViewComponent,
                 private gnathub: GNAThubService,
                 private cdRef: ChangeDetectorRef,
                 private http: Http) {}

    /** @override */
    public ngOnInit(): void {
        this.sub = this.route.params.subscribe(params => {
            let filename: string = params['filename'];
            let line: number = params['line'];
            let id: number = params['id'];

            this.selectedLine = line;
            this.selectedId = id;

            this.reportService.page = filename;
            if (filename && filename !== this.sourceView.filename){
                this.reloadFile(filename, line, id);
            } else {
                this.processSource();
            }
        });
    }

    private reloadFile(filename: string, line: number, id: number): void {
        this.gnathub.getSource(filename).subscribe(
            blob => {
                this.source = blob;
                this.sourceView.filename = filename;
                this.processSource();
            },
            error => console.log('[Error] reloadFile :', error));
    }

    private processSource(): void {
        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || [];
        this.createInlineAnnotations();
        this.afterInitProcess(this.selectedLine, this.selectedId);
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
            if (this.reportService.checkArray(this.source.annotations,
                                              'annotated-source.component',
                                              'inlineAnnotations', 'source.annotations')) {
                this.source.annotations.forEach(function(
                                                annotation: IAnnotatedSourceMessage): void {
                    const toolId: number = annotation.rule.tool_id;
                    const line: number = annotation.line;
                    if (!this.inlineAnnotations.hasOwnProperty(line)) {
                        this.inlineAnnotations[line] = new Set();
                    }
                    this.inlineAnnotations[line].add(annotation);
                }.bind(this));
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

    private initSelectMsg(id: number): void {
        if (this.reportService.message){
            if (id !== -1
                && this.reportService.checkArray(this.reportService.message.sources,
                                                 'annotated-source.component',
                                                 'initSelectMsg',
                                                 'reportService.message.sources')) {
                this.reportService.message.sources.forEach(function(
                                                           source: ISourceNav): void {
                    if (source.filename === this.source.filename) {
                        let index: number = this.getIndex(id, source.messages);
                        this.selectMessage(source.messages[index]);
                    }
                }.bind(this));
            }
        } else  {
            console.log('[Error] annotated-source.component:initSelectMsg :'
                        + " reportService.message doesn't exist.");
        }
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    private goToLine(line: number): void {
        if (line) {
            this.selectedLine = line;
            let id: string = 'L' + line;

            const config: ScrollToConfigOptions = {
                target: id,
                offset: -270,
                duration: 200
            };

            let ret: any = this.scrollToService.scrollTo(config);
            if (ret.source === undefined){
                console.error('[Error] annotated-source.component:goToLine:'
                              + ' scrollToService failed.', ret);
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
        array.forEach(function(cell: any, idx: number): void {
            if (cell.id === id){
                index = idx;
            }
        });
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

    public selectMessage( message: any, event: KeyboardEvent): void {
        /* Keep these lines for future activation of the feature */
        /*if (event && event.ctrlKey && event.shiftKey && this.lastSelectedMsg != -1){
            if (this.reportService.checkArray(this.reportService.message.sources,
                                              'annotated-source.component',
                                              'selectMessage', 'reportService.message.sources')) {
                this.reportService.message.sources.forEach(function(source){
                    if (source.filename == this.source.filename) {

                        let tmp_id_idx = this.getIndex(message.id, source.messages);
                        let tmp_last_idx = this.getIndex(this.lastSelectedMsg, source.messages);

                        let start = (tmp_id_idx > tmp_last_idx ? tmp_last_idx + 1 : tmp_id_idx);
                        let end = (tmp_id_idx > tmp_last_idx ? tmp_id_idx : tmp_last_idx - 1);

                        while (start <= end){
                            this.toggleSelectedMsg(source.messages[start].id);
                            this.checkMessage(source.messages[start])
                            start ++;
                        }
                    }
                }.bind(this));
            }
        } else*/
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

        if (this.reportService.checkArray(this.reportService.message.sources,
                                          'annotated-source.component',
                                          'refreshFilter', 'reportService.message.sources')) {
            this.reportService.message.sources.forEach(function(
                                                       source: ISourceNav): void {
                if (source.messages){
                    source.messages.forEach(function(message: IMessage): void {
                        if (message.user_review){
                            userReviewFilter = this.reportService.putInFilter(message.user_review,
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
            this.reportService.countUncategorized(userReviewFilter,
                                                  this.reportService.filter._total_message_count);
        }

        this.reportService.filter.review_status = userReviewFilter;
        updateFilter(this.reportService);
    }

    public addDynamicReview(newReview: any[]): void {
        if (this.reportService.checkArray(this.reportService.message.sources,
                                          'annotated-source.component',
                                          'addDynamicReview', 'reportService.message.sources')) {
            this.reportService.message.sources.forEach(function(source: ISourceNav): void{
                if (source.messages){
                    source.messages.forEach(function(message: IMessage): void {
                        if (newReview[message.tool_msg_id]){
                            if (!message.review_history) {
                                message.review_history = [];
                            }
                            message.review_history.unshift(newReview[message.tool_msg_id]);
                            message.user_review = newReview[message.tool_msg_id];
                        }
                    });
                }
            });
            this.refreshFilter();
        }
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

    public writeReview(): void {
        if (!this.checkedMsg || this.checkedMsg.length === 0) {return; }
        this.dialog.review().subscribe((data: any) => {

            if (!data) {return; }

            /*Timestamp format must be YYYY-MM-jj HH:mm:ss*/
            let now: Date = new Date();

            let year: string = now.getFullYear().toString();
            let month: string = this.formatDate(now.getMonth() + 1);
            let day: string = this.formatDate(now.getDate());
            let hour: string = this.formatDate(now.getHours());
            let minutes: string = this.formatDate(now.getMinutes());
            let seconds: string = this.formatDate(now.getSeconds());

            let date: string = year + '-' + month + '-' + day + ' '
                               + hour + ':' + minutes + ':' + seconds;

            /*Now create xml*/
            let xml: string = "<?xml version='1.0' encoding='utf-8'?>\n<audit_trail format='6'>\n";
            let newReview: any[] = [];

            if (this.reportService.checkArray(this.checkedMsg,
                                              'annotated-source.component',
                                              'writeReview', 'checkedMsg')) {
                this.checkedMsg.forEach(function(id: number): void {

                    /* Create the xml to send to codepeer_bridge */
                    xml += "<message identifier='" + id + "'>\n";
                    xml += "<audit timestamp='" + date + "' ";

                    /* Create the object to add to the client side to show it */
                    newReview[id] = {};
                    newReview[id].date = date;

                    if (data.status) {
                        xml += "status='" + data.status + "' ";
                        xml += "status_category='" + data.category + "' ";
                        newReview[id].status = data.status;
                        newReview[id].display_name = this.formatName(data.status);
                        newReview[id].status_kind = data.category;
                    }
                    if (data.username) {
                        xml += "approved='" + data.username + "' ";
                        newReview[id].author = data.username;
                    }
                    xml += "from_source='FALSE'>";
                    if (data.review) {
                        xml += data.review;
                        newReview[id].message = data.review;
                    }
                    xml += '</audit>\n</message>\n';

                }.bind(this));
            }
            xml += '</audit_trail>';

            this.sendUserReview(xml, newReview);
            this.selectedLine = -1;
            this.checkedMsg = [];
            this.selectedMsg = [];
            this.reportService.selectedMessage = [];
        });
    }

    public sendUserReview(xml: string, newReview: IReviewUser[]): void {
        let url: string = this.reportService.url + 'post-review/';
        this.http.post(url, xml)
            .subscribe(data => {
            this.addDynamicReview(newReview);
            this.reportService.refreshUserReview();
        }, error => {
            console.error('[Error] sendUserReview :', error);
            this.reportService.errorToShow.push('Error when trying to add a review.');
            this.reportService.verifyServerStatus();
        });
    }

    public trackMsg(index: number, message: any): void {
        return message ? message.id : undefined;
    }

    public trackSrc(index: number, source: any): void{
        return source ? source.filename : undefined;
    }

    public sortModules(firstSort: string, secondSort: string): void {
        let newFilter: ISort = {newSort: firstSort, otherSort: secondSort};
        this.reportService.message.sources = sortMessageArray(
            newFilter,
            this.reportService.messageSort,
            this.reportService.message.sources);
        storeMessageSort(newFilter);
    }

}
