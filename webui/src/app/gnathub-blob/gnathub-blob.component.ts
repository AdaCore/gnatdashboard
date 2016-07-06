import { CORE_DIRECTIVES } from '@angular/common';
import { Component, ViewEncapsulation } from '@angular/core';
import { DomSanitizationService, SafeHtml } from '@angular/platform-browser';
import { ROUTER_DIRECTIVES, Router } from '@angular/router';

import { IGNAThubBlob, IGNAThubBlobLine } from 'gnat';
import { highlightAuto } from 'highlight.js';
import { Subscription } from 'rxjs/Subscription';

import { highlightAda } from '../ada-lang';
import { unescapeHTML } from '../html-utils';
import { PathEncoder } from '../path-encoder';
import { ReportService } from '../report.service';

@Component({
    selector: 'gnathub-blob',
    encapsulation: ViewEncapsulation.None,
    templateUrl: './gnathub-blob.template.html',
    directives: [ CORE_DIRECTIVES, ROUTER_DIRECTIVES ],
    providers: [ ReportService ]
})
export class GNAThubBlob extends PathEncoder {
    private filename: string = null;
    private blob: IGNAThubBlob = null;
    private htmlLines: { number: number, content: SafeHtml }[] = null;
    private htmlLinesOfBlob: IGNAThubBlob = null;
    private sub: Subscription = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
     */
    constructor(
        private reportService: ReportService,
        private router: Router,
        private sanitizer: DomSanitizationService)
    {
        super();
    }

    /**
     * Query the annotated source data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.sub = this.router
            .routerState
            .queryParams
            .subscribe(params => {
                this.filename = params.hasOwnProperty('filename') ?
                    this.decodePath(params['filename']) : null;
                if (this.filename) {
                    this.reportService.GNAThubBlob(
                        this.filename,
                        (blob: IGNAThubBlob) => {
                            if (this.blob !== blob) {
                                this.blob = blob;
                            }
                        });
                }
            });
    }

    /** @override */
    ngOnDestroy() {
        this.sub.unsubscribe();
    }

    /**
     * Mark the input string as safe HTML (for use with [innerHTML]).
     *
     * @param value The input string.
     * @return The safe HTML.
     */
    private bypassSanitizer(value: string): SafeHtml {
        return this.sanitizer.bypassSecurityTrustHtml(value)
    }

    /**
     * Highlight the whole file and return an array of lines.
     *
     * @return An array of HTML string with highlighting markup, or |null| if
     *      the blob is not loaded.
     */
    public highlightedLines(): { number: number, content: SafeHtml }[] {
        if (!this.blob || !this.blob.lines) {
            return [];
        }
        if (!this.htmlLines || this.htmlLinesOfBlob !== this.blob) {
            let i = 0;
            this.htmlLines = this.highlight(
                this.blob.lines.map(l => l.content).join('')
            ).split(/\r\n|\r|\n/g).map(
                (content: string) => {
                    return {
                        number: ++i,
                        content: this.bypassSanitizer(`${content}\n`)
                    };
                });
            this.htmlLinesOfBlob = this.blob;
        }
        return this.htmlLines;
    }

    /**
     * Syntax highlight a code snippet.
     *
     * @param code The source code to highlight.
     * @return The HTML string with highlighting markup.
     */
    public highlight(code: string): string {
        // TODO(delay): avoid having to deal with encoded HTML entities at this
        // point (ie. remove the call to |unescapeHTML|).
        if (this.filename.endsWith('.ads') || this.filename.endsWith('.adb')) {
            return highlightAda(code);
        }
        return highlightAuto(code).value;
    }
}
