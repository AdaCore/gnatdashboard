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
import { ScrollToService, ScrollToConfigOptions } from '@nicky-lenaers/ngx-scroll-to';


import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
    IReviewFilter
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
    public tabOpen = 'message';
    public checked_msg: Array<number> = [];
    public selected_msg: Array<number> = [];
    private last_selected_msg: number = -1;
    private sub: Subscription;
    public selectedLine: number;
    public selectedId: number;

    @ViewChild('scrollView', {static: true}) private scrollView: ElementRef;

    constructor(
    @Inject(DOCUMENT) private document: Document,
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
    public ngOnInit() {

        this.sub = this.route.params.subscribe(params => {
            let filename = params['filename'];
            let line = params['line'];
            let id = params['id'];

            this.selectedLine = line;
            this.selectedId = id;

            this.reportService.page = filename;
            if (filename && filename != this.sourceView.filename){
                this.reloadFile(filename, line, id);
            } else {
                this.processSource();
            }
        });
    }



    private reloadFile(filename, line, id){
        this.gnathub.getSource(filename).subscribe(
            blob => {
                this.source = blob;
                this.sourceView.filename = filename;
                this.processSource();
            },
            error => console.log("[Error] reloadFile :", error));
    }

    private processSource(){
        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || [];
        this.createInlineAnnotations();
        this.afterInitProcess(this.selectedLine, this.selectedId);
    }

    private afterInitProcess(line, id) {
        if (line){
            this.goToLine(line);
        }
        if (id){
            this.initSelectMsg(id);
        }
    }

    private createInlineAnnotations(){
        // Prepare inline annotations.
        this.inlineAnnotations = {};
        this.source.annotations = this.source.annotations || [];
        if (this.source) {
            if (this.reportService.checkArray(this.source.annotations,
                                              "annotated-source.component",
                                              "inlineAnnotations", "source.annotations")) {
                this.source.annotations.forEach(function(annotation){
                    const toolId = annotation.rule.tool_id;
                    const line = annotation.line;
                    if (!this.inlineAnnotations.hasOwnProperty(line)) {
                        this.inlineAnnotations[line] = new Set();
                    }
                    this.inlineAnnotations[line].add(annotation);
                }.bind(this));
            }
        } else {
            console.log("[Error] annotated-source.component:inlineAnnotations : source doesn't exist.");
        }
    }

    /** @override */
    public ngAfterViewInit() {
        switch (document.readyState) {
                /*            case 'incomplete':
                console.log("[Error] annotated-source : afterViewInit : Document not loaded : scrolling to line impossible");
                break;*/
            case 'interactive':
                console.log("[Warning] annotated-source : afterViewInit : Document still loading : scrolling can fail");
                this.afterInitProcess(this.selectedLine, this.selectedId);
                break;
            case 'complete':
                this.afterInitProcess(this.selectedLine, this.selectedId);
                break;
        }
        this.cdRef.detectChanges();
    }

    /** @override */
    public ngOnDestroy() {
        if (this.sub) {
            this.sub.unsubscribe();
        }
    }

    private initSelectMsg(id: number) {
        if (this.reportService.message){
            if (id != -1 && this.reportService.checkArray(this.reportService.message.sources,
                                                          "annotated-source.component",
                                                          "initSelectMsg", "reportService.message.sources")) {
                this.reportService.message.sources.forEach(function(source){
                    if (source.filename == this.source.filename) {
                        let index = this.getIndex(id, source.messages);
                        this.selectMessage(source.messages[index]);
                    }
                }.bind(this));
            }
        } else  {
            console.log("[Error] annotated-source.component:initSelectMsg : reportService.message doesn't exist.");
        }
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    private goToLine(line: number) {
        if (line) {
            this.selectedLine = line;
            line = line - 10 > 0 ? line - 10 : 1;
            let id = "L"+line;

            const config: ScrollToConfigOptions = {
                target: id
            };
            this.scrollToService.scrollTo(config);
        }
    };

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public changeFile(filename: string, file_line: number, id: number) {
        this.router.navigate(['/source', filename, { line: file_line, id: id }]);
    }

    public checkMessage(message: any) {
        if (message){
            let isCodepeerMsg = (this.reportService.codepeer_code == message.rule.tool_id ? true : false);
            if (isCodepeerMsg){
                if (this.checked_msg.length == 0) {
                    this.reportService.selectedMessage = [];
                }
                let index = this.checked_msg.indexOf(message.tool_msg_id);
                if (index != -1) {
                    this.checked_msg.splice(index, 1);
                    this.reportService.selectedMessage.splice(index, 1);
                } else {
                    this.checked_msg.push(message.tool_msg_id);
                    this.reportService.selectedMessage.push(message);
                }
            }
        }
    }

    private getIndex(id, array){
        let index = -1;
        array.forEach(function(cell, idx){
            if (cell.id == id){
                index = idx;
            }
        });
        return index;
    }

    private toggleSelectedMsg(id) {
        let tmp = this.selected_msg.indexOf(id);
        if (tmp == -1) {
            this.selected_msg.push(id);
        } else {
            this.selected_msg.splice(tmp, 1);
        }
    }

    public selectMessage( message: any, event: KeyboardEvent) {
        /* Keep these lines for future activation of the feature */
        /*if (event && event.ctrlKey && event.shiftKey && this.last_selected_msg != -1){
            if (this.reportService.checkArray(this.reportService.message.sources,
                                              "annotated-source.component",
                                              "selectMessage", "reportService.message.sources")) {
                this.reportService.message.sources.forEach(function(source){
                    if (source.filename == this.source.filename) {

                        let tmp_id_idx = this.getIndex(message.id, source.messages);
                        let tmp_last_idx = this.getIndex(this.last_selected_msg, source.messages);

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
        } else*/ if (event && event.ctrlKey && message) {
            this.toggleSelectedMsg(message.id);
            this.checkMessage(message);
            this.selectedLine = -1;
        } else {
            this.checked_msg = [];
            this.selected_msg = [];
            this.reportService.selectedMessage = [];
            this.checkMessage(message);
            if (message){
                this.goToLine(message.line);
                this.toggleSelectedMsg(message.id);
            }
        }
        if (message){
            this.last_selected_msg = message.id;
        }
    }

    public showReviewsChanges() {
        this.reportService.showReviews = !this.reportService.showReviews
    }

    public ShowReviewHistory(message: any) {

        this.reportService.history.reviews = message.review_history;
        this.reportService.history.message = message;
        this.dialog.reviewHistory().subscribe((data:any) => {});
    }

    private refreshFilter() {
        let userReviewFilter: [IReviewFilter];

        if (this.reportService.checkArray(this.reportService.message.sources,
                                          "annotated-source.component",
                                          "refreshFilter", "reportService.message.sources")) {
            this.reportService.message.sources.forEach(function(source){
                if (source.messages){
                    source.messages.forEach(function(message){
                        if (message.user_review){
                            userReviewFilter = this.reportService.putInFilter(message.user_review, userReviewFilter);
                        }
                    }.bind(this))
                }

            }.bind(this));
        }

        let count = this.reportService.countUncategorized(userReviewFilter, this.reportService.filter._total_message_count);
        if (count > 0){
            let tmp_review = {
                status: "UNCATEGORIZED",
                display_name: "Uncategorized"
            };
            this.reportService.putInFilter(tmp_review, userReviewFilter);
            this.reportService.countUncategorized(userReviewFilter, this.reportService.filter._total_message_count);
        }

        this.reportService.filter.review_status = userReviewFilter;
        updateFilter(this.reportService);
    }

    public addDynamicReview(new_review) {
        if (this.reportService.checkArray(this.reportService.message.sources,
                                          "annotated-source.component",
                                          "addDynamicReview", "reportService.message.sources")) {
            this.reportService.message.sources.forEach(function(source){
                if (source.messages){
                    source.messages.forEach(function(message){
                        if (new_review[message.tool_msg_id]){
                            if (!message.review_history) {
                                message.review_history = [];
                            }
                            message.review_history.unshift(new_review[message.tool_msg_id]);
                            message.user_review = new_review[message.tool_msg_id];
                        }
                    })
                }
            });
            this.refreshFilter();
        }
    }

    private formatName(name:string): string {
        let myArray = name.split('_');
        let display_name = '';
        myArray.forEach(function(word, idx){
            display_name += word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
            if (idx < myArray.length - 1){ display_name += ' ';}
        });

        return display_name;
    }

    private formatDate(int: number): string {
        return int < 10 ? '0' + int.toString() : int.toString();
    }

    public writeReview() {

        if (!this.checked_msg || this.checked_msg.length == 0) {return }
        this.dialog.review().subscribe((data: any) => {

            if (!data) {return }

            /*Timestamp format must be YYYY-MM-jj HH:mm:ss*/
            let now = new Date();
            var year = now.getFullYear().toString();
            var month = this.formatDate(now.getMonth());
            var day = this.formatDate(now.getDate());
            var hour = this.formatDate(now.getHours());
            var minutes = this.formatDate(now.getMinutes());
            var seconds = this.formatDate(now.getSeconds());

            let date = year + "-" + month + "-" + day + " " + hour + ":" + minutes + ":" + seconds;

            /*Now create xml*/
            let xml = '<?xml version="1.0" encoding="utf-8"?>\n<audit_trail format="6">\n';
            let new_review = [];

            if (this.reportService.checkArray(this.checked_msg,
                                              "annotated-source.component",
                                              "writeReview", "checked_msg")) {
                this.checked_msg.forEach(function(id){

                    /* Create the xml to send to codepeer_bridge */
                    xml += '<message identifier="' + id + '">\n';
                    xml += '<audit timestamp="' + date + '" ';

                    /* Create the object to add to the client side to show it */
                    new_review[id] = {};
                    new_review[id].date = date;

                    if (data.status) {
                        xml += 'status="' + data.status + '" ';
                        xml += 'status_category="' + data.category + '" ';
                        new_review[id].status = data.status;
                        new_review[id].display_name = this.formatName(data.status);
                    }
                    if (data.username) {
                        xml += 'approved="' + data.username + '" ';
                        new_review[id].author = data.username;
                    }
                    xml += 'from_source="FALSE">';
                    if (data.review) {
                        xml += data.review;
                        new_review[id].message = data.review;
                    }
                    xml += '</audit>\n</message>\n';

                }.bind(this));
            }
            xml += '</audit_trail>';

            this.sendUserReview(xml, new_review);
            this.selectedLine = -1;
            this.checked_msg = [];
            this.selected_msg = [];
            this.reportService.selectedMessage = [];
        });
    }

    public sendUserReview(xml, new_review) {
        let url = this.reportService.url + "post-review/";
        this.http.post(url, xml)
            .subscribe(data => {
            this.addDynamicReview(new_review);
        }, error => {
            console.error("[Error] sendUserReview :", error);
            this.reportService.errorToShow.push("Error when trying to add a review.");
            this.reportService.verifyServerStatus();
        });
    }

    public trackMsg(index, message){
        return message ? message.id: undefined;
    }

    public trackSrc(index, source){
        return source ? source.filename: undefined;
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.message.sources = sortMessageArray(
            {newSort: firstSort, otherSort: secondSort},
            this.reportService.messageFilter,
            this.reportService.message.sources);
    }

}
