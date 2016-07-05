import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Observable';

import {
    IGNATcoverageHunk, IGNATcoverageReport, IGNAThubReport, IGNAThubBlob
} from 'gnat';

import 'rxjs/add/operator/map';

@Injectable()
export class ReportService {
    private mainRequest: Observable<Response> = null;
    private reportRequest: Observable<Response> = null;
    private hunkRequest: Observable<Response> = null;

    private blobRequests: { [filename: string]: Observable<Response> } = {};
    private blobs: { [filename: string]: IGNAThubBlob } = {};

    private gnathubReport: IGNAThubReport = null;
    private gnatcovReport: IGNATcoverageReport = null;
    private annotatedHunk: IGNATcoverageHunk = null;

    constructor (private http: Http) {
        this.mainRequest = http.get("api/html-report/gnathub.report.json");
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
     * Execute |callback| whenever the blob is ready.
     *
     * @param filename The name of the file to fetch.
     * @param callback The callback function to call once the data is ready.
     */
    public GNAThubBlob(
        filename: string,
        callback: (report: IGNAThubBlob) => void): void
    {
        if (this.blobRequests.hasOwnProperty(filename)) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.blobRequests[filename].subscribe(
                () => callback(this.blobs[filename]));
        } else if (!this.blobs.hasOwnProperty(filename)) {
            // The request as not yet been initialized. Execute it and save the
            // Observable to handle multiple requests to the same resource.
            this.blobRequests[filename] =
                this.http.get(`api/html-report/${filename}.json`);
            this.blobRequests[filename].subscribe((res: Response) => {
                // TODO(charly): assert |this.blobs.hasOwnProperty(filename)|
                // evaluate to |false|.
                this.blobs[filename] = res.json();
                delete this.blobRequests[filename];
                callback(this.blobs[filename]);
            });
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.blobs[filename]);
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
