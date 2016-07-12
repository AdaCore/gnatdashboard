import { Component } from '@angular/core';
import { CORE_DIRECTIVES } from '@angular/common';
import { ActivatedRoute } from '@angular/router';

import {
    IGNATcoverageHunk, IGNATcoverageHunkMapping, IGNATcoverageHunkStatement
} from 'gnat';

import { highlightAda } from '../ada-lang';
import { unescapeHTML } from '../html-utils';
import { PathEncoder } from '../path-encoder';
import { ReportService } from '../report.service';

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.template.html',
    styleUrls: [ './annotated-source.style.css' ],
    directives: [ CORE_DIRECTIVES ],
    providers: [ ReportService ]
})
export class AnnotatedSource extends PathEncoder {
    private filename: string = null;
    private source: IGNATcoverageHunk = null;
    private SCO_RE: RegExp = /^SCO #([1-9][0-9]*): (.*)$/;

    /**
     * @param reportService Custom service to retrieve reports data.
     * @param routeParam The router service.
     */
    constructor(private reportService: ReportService, route: ActivatedRoute) {
        super();
        this.readRouteParameters(route.params);
    }

    /**
     * Query the annotated source data and store a reference to it.
     *
     * @override
     */
    public ngOnInit(): void {
        this.reportService.AnnotatedSource((hunk: IGNATcoverageHunk) => {
            this.source = hunk;
        });
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
        return highlightAda(unescapeHTML(code));
    }

    /**
     * Generate the violation message for the given mapping.
     *
     * @pre mapping.message is defined and not null
     * @return The message as a HTML string.
     */
    public formatMessage(mapping: IGNATcoverageHunkMapping): string {
        if (!mapping.message.SCO) {
            return mapping.message.message;
        }
        const match: string[] = this.SCO_RE.exec(mapping.message.SCO);
        if (!match) {
            return mapping.message.message;
        }
        const scoId: string = match[1];
        const kind: string = match[2].toLowerCase();
        const statement: IGNATcoverageHunkStatement = mapping.statements.find(
            (st: IGNATcoverageHunkStatement) => st.id === scoId);
        const colBegin: number = statement.range[0][1];
        const colEnd: number = statement.range[1][1];
        // col_begin and col_end are 1-based, but mapping.line.src is 0-based
        const code: string = mapping.line.src.slice(colBegin - 1, colEnd);

        return [
            `<span class="sco">${kind}</span>`,
            `<span class="blob-code">${code}</span>`,
            '<span class="blob-range">',
            `(between col ${colBegin} and ${colEnd})`,
            `</span><span>${mapping.message.message}</span>`
        ].join(" ");
    }

    /**
     * Update attributes wrt. route parameters.
     *
     * @param params An immutable map of parameters.
     */
    private readRouteParameters(params: { [key: string]: any }): void {
        this.filename = params.hasOwnProperty('filename') ?
            <string>this.decodePath(params['filename']) : null;
    }
}
