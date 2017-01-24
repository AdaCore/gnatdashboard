import {
    AfterViewInit, Component, ElementRef, Inject, Input, OnDestroy, OnInit,
    ViewChild
} from '@angular/core';
import { DOCUMENT, DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';

import {
    CoverageStatus, IGNAThubBlob, IGNAThubBlobLine, IGNAThubMessage,
    IGNAThubProperty, IGNAThubRule, IGNAThubTool
} from 'gnat';

import '../array/operator/sum';

import { PageScrollInstance, PageScrollService } from 'ng2-page-scroll';
import { Subscription } from 'rxjs';

type AttachedMessage = { line: IGNAThubBlobLine; msg: IGNAThubMessage };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ]
})
export class AnnotatedSourceComponent
    implements AfterViewInit, OnDestroy, OnInit
{
    @Input() public blob: IGNAThubBlob = null;
    public messages: AttachedMessage[] = [];
    public selectedLine: number = null;
    public displayMessages: boolean = true;
    public displayCoverage: boolean = true;

    private sub: Subscription = null;

    @ViewChild('scrollView') private scrollView: ElementRef;

    constructor(
        @Inject(DOCUMENT) private document: Document,
        private pageScrollService: PageScrollService,
        private route: ActivatedRoute,
        private sanitizer: DomSanitizer) {}

    public ngOnInit(): void {
        for (let toolId of Object.keys(this.blob.tools)) {
            // Show messages triggered by all tools by default
            this.blob.tools[toolId].ui_selected = true;
        }
        for (let ruleId of Object.keys(this.blob.rules)) {
            // Show messages triggered by all rules by default
            this.blob.rules[ruleId].ui_selected = true;
        }
        for (let propertyId of Object.keys(this.blob.properties)) {
            // Show all messages with properties by default
            this.blob.properties[propertyId].ui_selected = true;
        }

        if (this.blob.lines) {
            this.blob.lines.forEach(
                line => line.messages.forEach(
                    msg => this.messages.push({ line, msg })));
        }

        this.sub = this.route.params.subscribe(params => {
            this.selectedLine = +params['line'];
            this.goToLine(this.selectedLine);
        });
    }

    public ngAfterViewInit(): void {
        this.goToLine(this.selectedLine);
    }

    public ngOnDestroy(): void {
        this.sub.unsubscribe();
    }

    /**
     * @param metricId The identifier of the metric to look for.
     * @return Whether such a metric has been computed for this file.
     */
    public hasMetric(metricId: string): boolean {
        for (let metric of this.blob.metrics) {
            if (metric.rule.identifier === metricId) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param metricId The identifier of the metric to look for.
     * @return The value of the metric if found, |null| otherwise.
     */
    public getMetricValue(metricId: string): string {
        for (let metric of this.blob.metrics) {
            if (metric.rule.identifier === metricId) {
                return metric.message;
            }
        }
        return null;
    }

    /**
     * @return The total number of displayed messages.
     */
    public getMessageDisplayedCount(): number {
        return this.reduceMessages((count, message) => {
            return count + (this.shouldDisplayMessage(message) ? 1 : 0);
        });
    }

    /**
     * @param tool The tool which messages this function counts.
     * @return The total number of messages displayed for a given tool.
     */
    public toolMessageCount = (tool: IGNAThubTool): number => {
        return this.reduceMessages((count, message) => {
            if (tool.id === message.rule.tool.id &&
                this.blob.tools[message.rule.tool.id].ui_selected &&
                this.shouldDisplayMessage(message))
            {
                return count + 1;
            }
            return count;
        });
    }

    /**
     * @param rule The rule which messages this function counts.
     * @return The total number of messages displayed for a given rule.
     */
    public ruleMessageCount = (rule: IGNAThubRule): number => {
        return this.reduceMessages((count, message) => {
            if (rule.id === message.rule.id &&
                this.blob.rules[message.rule.id].ui_selected &&
                this.shouldDisplayMessage(message))
            {
                return count + 1;
            }
            return count;
        });
    }

    /**
     * @param property The property which messages this function counts.
     * @return The total number of messages displayed for a given property.
     */
    public propertyMessageCount = (property: IGNAThubProperty): number => {
        return this.reduceMessages((count, message) => {
            if (message.properties.some(p => p.id === property.id) &&
                this.blob.properties[property.id].ui_selected &&
                this.shouldDisplayMessage(message))
            {
                return count + 1;
            }
            return count;
        });
    }

    /**
     * @param message The message to display or not.
     * @return Whether we should display the message given the current selection
     *      of tools/rules/properties.
     */
    public shouldDisplayMessage(message: IGNAThubMessage): boolean {
        return this.blob.tools[message.rule.tool.id].ui_selected &&
            this.blob.rules[message.rule.id].ui_selected &&
            (!message.properties.length || message.properties.some(property => {
                return this.blob.properties[property.id].ui_selected;
            }));
    }

    /**
     * Mark the input string as safe HTML (for use with [innerHTML]).
     *
     * @param value The input string.
     * @return The safe HTML.
     */
    public bypassSanitizer(value: string): SafeHtml {
        return this.sanitizer.bypassSecurityTrustHtml(value);
    }

    /**
     *
     * @param line The line for which to check messages.
     * @return Whether some messages should be displayed.
     */
    public hasDisplayableMessage(line: number): boolean {
        if (!this.blob || !this.blob.lines) {
            return false;
        }
        return this.blob.lines[line - 1].messages.some(
            message => this.shouldDisplayMessage(message));
    }

    /**
     * Return the list of messages attached to the given line.
     *
     * @param line The line for which to list messages.
     * @return The list of messages.
     */
    public messagesAt(line: number): IGNAThubMessage[] {
        if (!this.blob || !this.blob.lines) {
            return [];
        }
        return this.blob.lines[line - 1].messages;
    }

    /**
     * Return the coverage value for for the given line.
     *
     * @param line The line for which to get coverage information.
     * @return The coverage value.
     */
    public coverageAt(line: number): { status: CoverageStatus; hits: number } {
        if (!this.displayCoverage || !this.blob || !this.blob.lines) {
            return null;
        }
        return this.blob.lines[line - 1].coverage || null;
    }

    /**
     * Short-hand reduce operation on all messages of the file.
     *
     * @param callback Function to invoke on each message of the file.
     * @param initialValue Optional. Value to use as the first argument to the
     *      first call of the |callback|. Defaults to |0|.
     * @return The value that results from the reduction.
     */
    private reduceMessages(
        callback: (count: number, line: IGNAThubMessage) => number,
        initialValue: number = 0): number
    {
        if (!this.blob || !this.blob.lines) {
            return 0;
        }
        return this.blob.lines.reduce((count, line) => {
            return count +
                this.messagesAt(line.number).reduce(callback, initialValue);
        }, 0 /* initialValue */);
    }

    private goToLine(line: number): void {
        let scroll: PageScrollInstance =
            PageScrollInstance.simpleInlineInstance(
                this.document, `#L${line}`, this.scrollView.nativeElement);
        this.pageScrollService.start(scroll);
    };
}
