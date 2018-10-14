import { Injectable } from '@angular/core';
import { Http, Response } from '@angular/http';
import { Observable } from 'rxjs/Observable';
import xml2js from 'xml2js'

import { IAnnotatedSourceFile, IFilterIndex, ICodeIndex, IMessageIndex } from 'gnat';

import 'rxjs/add/operator/map';
import 'rxjs/add/operator/catch';

@Injectable()
export class GNAThubService {
    public name: string = 'test';

    constructor(private http: Http) {}

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
    public getReview(): Observable<any> {
        return this.http.get('data/codepeer_review.xml')
            .map(this.convertToJson)
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

    private createDisplayName(name){
        let myArray = name.split('_');
        let display_name = '';
        myArray.forEach(function(word, idx){
            display_name += word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
            if (idx < myArray.length - 1){ display_name += ' ';}
        });
        return display_name;
    }

    private createReview(review){
        let myReview = [];
        let lastReview = {
            author : '',
            status: '',
            date: 0,
            from_source: '',
            message: '',
            display_name: ''
        };
        if (review.audit.$){
            let tmpReview = {
                author : review.audit.$.approved,
                status: review.audit.$.status,
                date: review.audit.$.timestamp,
                from_source: review.audit.$.from_source,
                message: review.audit._,
                display_name: this.createDisplayName(review.audit.$.status)
            };
            myReview.push(tmpReview)
            lastReview = tmpReview;
        } else if (review.audit[0].$) {
            review.audit.forEach(function(tmp){
                let tmpReview = {
                    author : tmp.$.approved,
                    status: tmp.$.status,
                    date: tmp.$.timestamp,
                    from_source: tmp.$.from_source,
                    message: tmp._,
                    display_name: this.createDisplayName(tmp.$.status)
                };
                myReview.push(tmpReview)
                if (lastReview.date < tmpReview.date) {
                    lastReview = tmpReview;
                }
            }.bind(this));


        } else {
            console.log("[Error] convertToJson : Problem to add this user_review : ", review);
        }
        let tmp = {
            review_history: myReview,
            user_review: lastReview
        };
        return tmp;
    }

    public convertToJson(res: any) {
        let user_review = [];
        xml2js.parseString(res._body, { explicitArray: false }, (error, result) => {
            if (error) {
                console.log("[Error] convertToJson : Problem to parse :", error);
            } else {
                if (result && result.audit_trail && result.audit_trail.message && result.audit_trail.message.length > 0){
                    result.audit_trail.message.forEach(function(review){
                        let tmp = this.createReview(review);
                        user_review[review.$.identifier] = tmp;
                    }.bind(this));
                } else if (result && result.audit_trail && result.audit_trail.message && result.audit_trail.message.audit) {
                    let tmp = this.createReview(result.audit_trail.message);
                    user_review[result.audit_trail.message.$.identifier] = tmp;
                } else {
                    console.log("[Error] gnathub.service:convertToJson : result.audit_trail.message should not be empty.");
                }
            }
        });
        return user_review;
    }
}
