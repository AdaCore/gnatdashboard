import { CORE_DIRECTIVES } from '@angular/common';
import { Component } from '@angular/core';
import { DomSanitizationService, SafeHtml } from '@angular/platform-browser';
import { ROUTER_DIRECTIVES, Router } from '@angular/router';

import { IGNAThubBlob, IGNAThubBlobLine, IGNAThubMessage } from 'gnat';
import { highlightAuto } from 'highlight.js';
import { Subscription } from 'rxjs/Subscription';

import { Loader } from '../loader/loader.component';
import { PathEncoder } from '../path-encoder';
import { ReportService } from '../report.service';

@Component({
    selector: 'gnathub-blob',
    templateUrl: './gnathub-blob.template.html',
    styleUrls: [ './gnathub-blob.style.css' ],
    directives: [ CORE_DIRECTIVES, Loader, ROUTER_DIRECTIVES ],
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
        this.sub = null;
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
    highlightedLines(): { number: number, content: SafeHtml }[] {
        if (!this.blob || !this.blob.lines) {
            return [];
        }
        if (!this.htmlLines || this.htmlLinesOfBlob !== this.blob) {
            // Decorate the code with HTML markup for highlighting.
            // Pass all lines to |this.highlight| as a single string to provide
            // the highlight engine with as much context as possible (for
            // markups that span on several lines, eg. Python docstrings).
            // NOTE: The |string.replace| method is used to remove the extra
            // line otherwise introduced by the |string.split| method:
            //
            //      > 'line 1\nline 2\nline 3\n'.split(/\r\n|\r|\n/g)
            //      [ 'line 1', 'line 2', 'line 3', '' ]
            //
            // This ensures this method returns a consistent line count (so that
            // we don't need to check the line exists in |this.messages|.
            let i = 0;
            this.htmlLines = this.highlight(
                this.blob.lines.map(l => l.content).join('')
            ).replace(/(\r\n|\r|\n)$/, '').split(/\r\n|\r|\n/g).map(
                (content: string) => {
                    return {
                        number: ++i,
                        content: this.bypassSanitizer(content)
                    };
                });
            this.htmlLinesOfBlob = this.blob;
        }
        return this.htmlLines;
    }

    /**
     * Return the list of messages attached to the given line.
     *
     * @param line The line for which to list messages.
     * @return The list of messages.
     */
    messages(line: number): IGNAThubMessage[] {
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
    coverage(line: number): string {
        if (!this.blob || !this.blob.lines) {
            return '';
        }
        return this.blob.lines[line - 1].coverage;
    }

    /**
     * Syntax highlight a code snippet.
     *
     * @param code The source code to highlight.
     * @return The HTML string with highlighting markup.
     */
    highlight(code: string): string {
        return highlightAuto(code).value;
    }
}
