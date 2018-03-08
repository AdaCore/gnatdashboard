import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Observable';

import { IReportIndex, IAnnotatedSourceFile } from 'gnat';

import { IFilterIndex, ICodeIndex, IMessageIndex } from 'gnat';

import 'rxjs/add/operator/map';
import 'rxjs/add/operator/catch';

@Injectable()
export class GNAThubService {
    constructor(private http: Http) {}

    public getReport(): Observable<IReportIndex> {
        return this.http.get('data/report.json')
            .map(this.handleResults)
            .catch(this.handleError);
    }
    public getFilter(): Observable<IFilterIndex> {
        return this.http.get('data/filter.json')
            .map(this.handleResults)
            .catch(this.handleError);
    }
    public getCode(): Observable<ICodeIndex> {
        return this.http.get('data/code.json')
            .map(this.handleResults)
            .catch(this.handleError);
    }
    public getMessage(): Observable<IMessageIndex> {
        return this.http.get('data/message.json')
            .map(this.handleResults)
            .catch(this.handleError);
    }

    public getSource(filename): Observable<IAnnotatedSourceFile> {
        return this.http.get(`data/src/${filename}.json`)
            .map(this.handleResults)
            .catch(this.handleError);
    }

    private handleResults(res: Response) {
        return res.json() || {};
    }

    private handleError(error: any) {
        let errMsg = (error.message) ?
            error.message : error.status ?
            `${error.status} - ${error.statusText}` : 'Server error';
        console.error(errMsg);
        return Observable.throw(errMsg);
    }
}
