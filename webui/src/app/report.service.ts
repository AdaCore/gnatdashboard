import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Observable';

import { IGNAThubReport, IGNAThubBlob } from 'gnat';

import 'rxjs/add/operator/map';

@Injectable()
export class ReportService {
    private mainRequest: Observable<Response> = null;

    private blobRequests: { [filename: string]: Observable<Response> } = {};
    private blobs: { [filename: string]: IGNAThubBlob } = {};

    private gnathubReport: IGNAThubReport = null;

    constructor(private http: Http) {
        this.mainRequest = http.get('data/gnathub.report.json');
        this.mainRequest.subscribe((res: Response) => {
            this.mainRequest = null;
            this.gnathubReport = res.json()
        });
    }

    /**
     * Execute |callback| whenever the report is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public GNAThubReport(callback: (report: IGNAThubReport) => void): void {
        if (this.mainRequest) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.mainRequest.subscribe(
                () => callback(this.gnathubReport),
                () => callback(null)
            );
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
                () => callback(this.blobs[filename]),
                () => callback(null)
            );
        } else if (!this.blobs.hasOwnProperty(filename)) {
            // The request as not yet been initialized. Execute it and save the
            // Observable to handle multiple requests to the same resource.
            this.blobs[filename] = null;
            this.blobRequests[filename] =
                this.http.get(`data/src/${filename}.json`);
            this.blobRequests[filename].subscribe(
                (res: Response) => {
                    this.blobs[filename] = res.json();
                    delete this.blobRequests[filename];
                    callback(this.blobs[filename]);
                },
                () => {
                    delete this.blobRequests[filename];
                    callback(null);
                });
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.blobs[filename]);
        }
    }
}
