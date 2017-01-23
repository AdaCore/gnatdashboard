import {
    Component, ElementRef, Inject, OnInit, ViewChild
} from '@angular/core';
import { DOCUMENT, DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';

import { GNAThubService } from '../gnathub.service';
import {
    CoverageStatus, IGNAThubBlob, IGNAThubBlobLine, IGNAThubMessage,
    IGNAThubProperty, IGNAThubRule, IGNAThubTool
} from 'gnat';

import '../array/operator/sum';
import { PageScrollInstance, PageScrollService } from 'ng2-page-scroll';

type Annotation = { line: IGNAThubBlobLine; msg: IGNAThubMessage };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ]
})
export class AnnotatedSourceComponent implements OnInit {
    public filename: string = null;
    public blob: IGNAThubBlob = null;
    public isBlobFetchError: boolean = false;
    public annotations: Annotation[] = null;

    @ViewChild('scrollView') private scrollView: ElementRef;
    private filters: any = null;

    constructor(
        @Inject(DOCUMENT) private document: Document,
        private gnathub: GNAThubService,
        private pageScrollService: PageScrollService,
        private route: ActivatedRoute,
        private sanitizer: DomSanitizer) {}

    public ngOnInit(): void {
        this.filename = this.route.snapshot.params['filename'];
        this.gnathub.getSource(this.filename).subscribe(
            blob => {
                for (let toolId of Object.keys(blob.tools)) {
                    // Show messages triggered by all tools by default
                    blob.tools[toolId].ui_selected = true;
                }
                for (let ruleId of Object.keys(blob.rules)) {
                    // Show messages triggered by all rules by default
                    blob.rules[ruleId].ui_selected = true;
                }
                for (let propertyId of Object.keys(blob.properties)) {
                    // Show all messages with properties by default
                    blob.properties[propertyId].ui_selected = true;
                }
                let annotations: Annotation[] = [];
                if (blob.lines) {
                    blob.lines.forEach(line => line.messages.forEach(msg => {
                        annotations.push({ line, msg });
                    }));
                }
                this.blob = blob;
                this.annotations = annotations;
            },
            error => this.isBlobFetchError = !!error);
    }

    public goToLine(line: number): void {
        let scroll: PageScrollInstance = PageScrollInstance.simpleInlineInstance(
            this.document, `#L${line}`, this.scrollView.nativeElement);
        this.pageScrollService.start(scroll);
    };

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
    public messages(line: number): IGNAThubMessage[] {
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
    public coverage(line: number): { status: CoverageStatus; hits: number } {
        if (!this.blob || !this.blob.lines) {
            return null;
        }
        return this.blob.lines[line - 1].coverage || { status: '', hits: 0 };
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
                this.messages(line.number).reduce(callback, initialValue);
        }, 0 /* initialValue */);
    }
}
