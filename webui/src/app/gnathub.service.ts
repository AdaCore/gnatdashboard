import {throwError as observableThrowError, Observable } from 'rxjs';
import {map, catchError} from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import xml2js from 'xml2js';

import { IAnnotatedSourceFile, IFilterIndex, ICodeIndex,
        IMessageIndex, IReviewUser } from 'gnat';
import { createDisplayName } from './utils/createDisplayName';

@Injectable()
export class GNAThubService {
    public name: string = 'test';

    constructor(private http: Http) {}

    public getFilter(): Observable<IFilterIndex> {
        return this.http.get('data/filter.json').pipe(
            map(this.handleResults),
            catchError(this.handleError), );
    }
    public getCode(): Observable<ICodeIndex> {
        return this.http.get('data/code.json').pipe(
            map(this.handleResults),
            catchError(this.handleError), );
    }
    public getMessage(): Observable<IMessageIndex> {
        return this.http.get('data/message.json').pipe(
            map(this.handleResults),
            catchError(this.handleError), );
    }
    public getReview(): Observable<any> {
        return this.http.get('data/codepeer_review.xml').pipe(
            map(this.convertToJson),
            catchError(this.handleError), );
    }

    public getSource(filename: string): Observable<IAnnotatedSourceFile> {
        return this.http.get(`data/src/${filename}.json`).pipe(
            map(this.handleResults),
            catchError(this.handleError));
    }

    public getCodepeerRun(): Observable<any> {
        return this.http.get('data/codepeer_run').pipe(
            map(this.handleResults),
            catchError(this.handleError));
    }

    private handleResults(res: Response): any {
        return res.json() || {};
    }

    private handleError(error: any): any {
        let errMsg: string = (error.message) ?
            error.message : error.status ?
            `${error.status} - ${error.statusText}` : 'Server error';
        console.error(errMsg);
        return observableThrowError(errMsg);
    }

    private createReview(review: any): any {
        let myReview: IReviewUser[] = [];

        let lastReview: IReviewUser = {
            author: '',
            status: '',
            status_priority: 0,
            status_kind: '',
            date: '',
            from_source: '',
            message: '',
            display_name: ''
        };
        if (review.audit.$){
            let displayName: string = createDisplayName(review.audit.$.status);

            let tmpReview: IReviewUser = {
                author: review.audit.$.approved,
                status: review.audit.$.status,
                status_priority: -1,
                status_kind: review.audit.$.status_category,
                date: review.audit.$.timestamp,
                from_source: review.audit.$.from_source,
                message: review.audit._,
                display_name: displayName
            };
            myReview.push(tmpReview);
            lastReview = tmpReview;
        } else if (review.audit[0].$) {
            review.audit.forEach(function(tmp: any): void {
                let displayName: string = createDisplayName(tmp.$.status);

                let tmpReview: IReviewUser = {
                    author: tmp.$.approved,
                    status: tmp.$.status,
                    status_priority: -1,
                    status_kind: tmp.$.status_category,
                    date: tmp.$.timestamp,
                    from_source: tmp.$.from_source,
                    message: tmp._,
                    display_name: displayName
                };
                myReview.push(tmpReview);
                if (lastReview.date < tmpReview.date) {
                    lastReview = tmpReview;
                }
            }.bind(this));
        } else {
            console.log('[Error] convertToJson : Problem to add this user_review : ', review);
        }
        let tmp: any = {
            review_history: myReview,
            user_review: lastReview
        };
        return tmp;
    }

    public convertToJson(res: any): IReviewUser[] {
        let userReview: IReviewUser[] = [];
        xml2js.parseString(res._body, { explicitArray: false }, (error, result) => {
            if (error) {
                console.log('[Error] convertToJson : Problem to parse :', error);
            } else {
                if (result && result.audit_trail && result.audit_trail.message
                    && result.audit_trail.message.length > 0){
                    result.audit_trail.message.forEach(function(review: any): void {
                        let tmp: any = this.createReview(review);
                        userReview[review.$.identifier] = tmp;
                    }.bind(this));
                } else if (result && result.audit_trail && result.audit_trail.message
                           && result.audit_trail.message.audit) {
                    let tmp: any = this.createReview(result.audit_trail.message);
                    userReview[result.audit_trail.message.$.identifier] = tmp;
                } else {
                    console.log('[Error] gnathub.service:convertToJson : '
                                + 'result.audit_trail.message should not be empty.');
                    console.log('Please see result : ', result);
                }
            }
        });
        return userReview;
    }
}
