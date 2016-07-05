import { Component, ViewEncapsulation } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ROUTER_DIRECTIVES, Router } from '@angular/router';

import { Subscription } from 'rxjs/Subscription';

import { IGNAThubBlob } from 'gnat';

import { highlight, highlightAda } from '../ada-lang';
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
    private sub: Subscription = null;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
     */
    constructor(private reportService: ReportService, private router: Router) {
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
                        (blob: IGNAThubBlob) => this.blob = blob);
                }
            });
    }

    /** @override */
    ngOnDestroy() {
        this.sub.unsubscribe();
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
            return highlightAda(unescapeHTML(code));
        }
        if (this.filename.endsWith('.py')) {
            return highlight(unescapeHTML(code), 'python');
        }
        if (this.filename.endsWith('.c') || this.filename.endsWith('.h')) {
            return highlight(unescapeHTML(code), 'C');
        }
        return unescapeHTML(code);
    }
}
