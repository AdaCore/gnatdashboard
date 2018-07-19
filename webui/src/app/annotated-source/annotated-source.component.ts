import {
    AfterViewInit,
    Component,
    ElementRef,
    HostBinding,
    Inject,
    Input,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';
import { Router } from '@angular/router';

import { PageScrollInstance, PageScrollService } from 'ng2-page-scroll';
import { Subscription } from 'rxjs';

import { SharedReport } from '../main-responder.service';

import { DialogsService } from './dialog.service'

import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
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
    public selectedLine: number = 0;
    public displayMessages: boolean = true;
    @HostBinding('class.with-coverage') public displayCoverage: boolean = true;
    public showMessageList: boolean = true;
    public inlineMessages: { [line: number]: MessagesByToolId };
    public inlineMessagesShownCount: number = 0;
    public inlineAnnotations: { [line: number]: IAnnotatedSourceMessage[] };
    public tabOpen = 'message';
    public checked_msg: Array<number> = [];
    private sub: Subscription;

    @ViewChild('scrollView') private scrollView: ElementRef;

    constructor(
    @Inject(DOCUMENT) private document: Document,
     private pageScrollService: PageScrollService,
     private route: ActivatedRoute,
     private router: Router,
     public reportService: SharedReport,
     public dialog: DialogsService) {}

    /** @override */
    public ngOnInit() {

        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || [];
        this.source.annotations = this.source.annotations || [];

        // Prepare inline messages.
        this.inlineMessages = {};
        if (this.source) {
            if (this.reportService.checkArray(this.source.messages,
                                              "annotated-source.component",
                                              "inlineMessages", "source.messages")) {
                this.source.messages.forEach(function(message){
                    const toolId = message.rule.tool_id;
                    const line = message.line;
                    if (!this.inlineMessages.hasOwnProperty(line)) {
                        this.inlineMessages[line] = {};
                    }
                    if (!this.inlineMessages[line].hasOwnProperty(toolId)) {
                        this.inlineMessages[line][toolId] = new Set();
                    }
                    this.inlineMessages[line][toolId].add(message);
                    this.inlineMessagesShownCount++;
                }.bind(this));
            }
        } else {
            console.log("[Error] annotated-source.component:inlineMessages : source doesn't exist.");
        }

        // Prepare inline annotations.
        this.inlineAnnotations = {};
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

                this.sub = this.route.params.subscribe(params => {
                    this.selectedLine = +params['line'];
                    this.goToLine(this.selectedLine);
                });
            }
        } else {
            console.log("[Error] annotated-source.component:inlineAnnotations : source doesn't exist.");
        }
    }

    /** @override */
    public ngAfterViewInit() {
        this.goToLine(this.selectedLine);
    }

    /** @override */
    public ngOnDestroy() {
        if (this.sub) {
            this.sub.unsubscribe();
        }
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    private goToLine(line: number) {
        if (line) {
            line = line - 10 > 0 ? line - 10 : 0;
            let scroll: PageScrollInstance =
                PageScrollInstance.simpleInlineInstance(
                    this.document, `#L${line}`, this.scrollView.nativeElement);
            this.pageScrollService.start(scroll);
        }
    };

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public changeFile(filename: string, file_line: number) {
        this.router.navigate(['/source', filename, { line: file_line }]);
        window.location.reload();
    }

    public checkMessage(id: number, message: any) {
        if (this.checked_msg.length == 0) {
            this.reportService.selectedMessage = [];
        }

        let index = this.checked_msg.indexOf(id);
        if (index != -1) {
            this.checked_msg.splice(index, 1);
            this.reportService.selectedMessage.splice(index, 1);
        } else {
            this.checked_msg.push(id);
            this.reportService.selectedMessage.push(message);
        }
    }

    public showReviewsChanges() {
        this.reportService.showReviews = !this.reportService.showReviews
    }

    public ShowReviewHistory(review_history: any) {

        this.reportService.history = review_history;
        this.dialog.reviewHistory().subscribe((data:any) => {});
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
        }
    }

    public writeReview() {

        if (!this.checked_msg || this.checked_msg.length == 0) {return }
        this.dialog.review().subscribe((data: any) => {

            if (!data) {return }

            let date = Date.now();
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
                        new_review[id].status = data.status;
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

                });
            }
            xml += '</audit_trail>';

            this.reportService.sendUserReview(xml);
            this.addDynamicReview(new_review);
        });
    }

}
