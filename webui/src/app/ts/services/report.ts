import { Injectable } from "angular2/core";
import { Http, Response } from "angular2/http";
import { Observable } from "rxjs/Observable";

import { IGNATcoverageReport } from "../model/gnatcoverage-report";

import 'rxjs/add/operator/map';

@Injectable()
export class ReportService {
    private req: Observable<Response> = null;
    private gnatcovReport: IGNATcoverageReport = null;

    constructor (http: Http) {
        this.req = http.get("api/report/gnatcoverage").map(
            (res: Response) => res.json()
        );
        this.req.subscribe((report: IGNATcoverageReport) => {
            this.gnatcovReport = report;
            this.req = null;
        });
    }

    /**
     * Execute |callback| whenever the report is ready.
     *
     * @param callback The callback function to call once the data is ready.
     */
    public GNATcovReport(callback: (report: IGNATcoverageReport) => void): void
    {
        if (this.req) {
            // The request is still processing. Subscribe to the response
            // |Observable| to get the parsed |events| object.
            this.req.subscribe(() => callback(this.gnatcovReport));
        } else {
            // The events have already been fetched and thus are available right
            // away. Call |callback| with no further ado.
            callback(this.gnatcovReport);
        }
    }
}
