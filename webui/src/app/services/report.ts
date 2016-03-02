import { Injectable } from "angular2/core";
import { Http, Response } from "angular2/http";
import { Observable } from "rxjs/Observable";

import {
    IGNATcoverageHunk, IGNATcoverageReport, IGNAThubReport
} from "gnat/reports";

import "rxjs/add/operator/map";

@Injectable()
export class ReportService {
    private mainRequest: Observable<Response> = null;
    private reportRequest: Observable<Response> = null;
    private hunkRequest: Observable<Response> = null;

    private gnathubReport: IGNAThubReport = null;
    private gnatcovReport: IGNATcoverageReport = null;
    private annotatedHunk: IGNATcoverageHunk = null;

    constructor (http: Http) {
        this.mainRequest = http.get("api/gnathub-report.json");
        this.mainRequest.subscribe(
            (res: Response) => this.gnathubReport = res.json());

        this.reportRequest = http.get("api/gnatcoverage-report.json");
        this.reportRequest.subscribe(
            (res: Response) => this.gnatcovReport = res.json());

        this.hunkRequest = http.get("api/zip_stream.adb.json");
        this.hunkRequest.subscribe(
            (res: Response) => this.annotatedHunk = res.json());
    }

    /**
     * Execute |callback| whenever the report is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public GNAThubReport(callback: (report: IGNAThubReport) => void): void
    {
        if (!this.gnathubReport) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.mainRequest.subscribe(() => callback(this.gnathubReport));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.gnathubReport);
        }
    }

    /**
     * Execute |callback| whenever the report is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public GNATcovReport(callback: (report: IGNATcoverageReport) => void): void
    {
        if (!this.gnatcovReport) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.reportRequest.subscribe(() => callback(this.gnatcovReport));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.gnatcovReport);
        }
    }

    /**
     * Execute |callback| whenever the annotated source is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public AnnotatedSource(callback: (hunk: IGNATcoverageHunk) => void): void {
        if (!this.annotatedHunk) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.hunkRequest.subscribe(() => callback(this.annotatedHunk));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.annotatedHunk);
        }
    }
}
