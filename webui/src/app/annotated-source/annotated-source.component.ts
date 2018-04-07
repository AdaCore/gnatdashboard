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

import { SharedReport } from '../main-responder.service'

import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
} from 'gnat';

type MessagesByToolId = { [toolId: number]: IAnnotatedSourceMessage[] };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ]
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
    public tabOpen = 'message';
    private sub: Subscription;

    @ViewChild('scrollView') private scrollView: ElementRef;

    constructor(
        @Inject(DOCUMENT) private document: Document,
        private pageScrollService: PageScrollService,
        private route: ActivatedRoute,
        private router: Router,
        public reportService: SharedReport) {}

    /** @override */
    public ngOnInit() {

        console.log("this.source ", this.source);
        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || [];

        // Prepare inline messages.
        this.inlineMessages = {};
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

        this.sub = this.route.params.subscribe(params => {
            this.selectedLine = +params['line'];
            this.goToLine(this.selectedLine);
        });
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

}
