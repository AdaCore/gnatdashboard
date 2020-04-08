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

    private get_srcpos(access: any, objectName: string, entry: string,
                       accessType: string, raceErrKind: string): any {
        let tmpAccessMap: any = {
            entry_point: entry,
            access: accessType,
            line: 0,
            file: '',
            err_kind: raceErrKind,
            obj_name: objectName
        };
        let raceCondition: any = [];
        if (access.srcpos.$) {
            tmpAccessMap.line = access.srcpos.$.line;
            tmpAccessMap.file = access.srcpos.$.file;
            raceCondition.push(tmpAccessMap);
        } else {
            access.srcpos.forEach(function(fileInfo: any): void{
                tmpAccessMap.line = fileInfo.$.line;
                tmpAccessMap.file = fileInfo.$.file;
                raceCondition.push(tmpAccessMap);
            });
        }
        return raceCondition;
    }

    public getAccessMap(accessMap: any, objName: string, raceErrKind: string): any {
        let entryPoint: string = accessMap.$.proc;
        let accesObj: any = [];

        if (accessMap.lock_access_entry.srcpos_set.$) {
            let access: any = accessMap.lock_access_entry.srcpos_set;
            let accessType: string = access.$.acc.replace(' access', '');
            accesObj = accesObj.concat(this.get_srcpos(access, objName, entryPoint,
                                                       accessType, raceErrKind));
        } else {
            accessMap.lock_access_entry.srcpos_set.forEach(function(access: any): void {
                let accessType: string = access.$.acc.replace(' access', '');
                accesObj = accesObj.concat(this.get_srcpos(access, objName, entryPoint,
                                                           accessType, raceErrKind));
            }.bind(this));
        }
        return accesObj;
    }

    private objInArray(object: any, arrayObject: any): boolean {
        let isInArray: boolean = false;
        arrayObject.forEach(function(refObject: any): void{
           if (object.entry_point === refObject.entry_point
              && object.line === refObject.line
              && object.access === refObject.access
              && object.err_kind === refObject.err_kind
              && object.file === refObject.file) {
               isInArray = true;
           }
        });
        return isInArray;
    }

    public raceToJson(raceXML: any): any {
        let raceJSON: any = {};
        xml2js.parseString(raceXML, { explicitArray: false }, (error, result) => {
            if (error) {
                console.log('[Error] raceToJson : Problem to parse :', error);
            } else {

                // Gather the information from the xml given by codepeer
                // into one array
                if (result && result.objs_in_trouble && result.objs_in_trouble.obj_race_info) {

                    let raceCondition: any = [];
                    result.objs_in_trouble.obj_race_info
                        .forEach(function(raceInfo: any): void {
                        let objName: string = raceInfo.$.obj_name;
                        let raceErrKind: string = raceInfo.$.race_err_kind;

                        if (raceInfo.lock_access_map.$) {
                            raceCondition = raceCondition.concat(this.getAccessMap(
                                            raceInfo.lock_access_map, objName, raceErrKind));
                        } else {
                            raceInfo.lock_access_map.forEach(function(accessMap: any): void {
                                raceCondition = raceCondition.concat(this.getAccessMap(
                                                accessMap, objName, raceErrKind));
                            }.bind(this));
                        }

                    }.bind(this));

                    // Parse the array, so we got
                    // a list of entry per object per file
                    raceCondition.forEach(function(obj: any): void{
                        let filename: string = obj.file.replace('-frameset.html', '');
                        let objName: string = obj.obj_name;
                        let tmpObj: any = {
                            entry_point : obj.entry_point,
                            line: obj.line,
                            access: obj.access,
                            err_kind: obj.err_kind,
                            file: filename
                        };
                        if (!raceJSON[objName]) {
                            raceJSON[objName] = [];
                        }
                        if (!this.objInArray(tmpObj,
                                            raceJSON[objName])) {
                            raceJSON[objName].push(tmpObj);
                        }
                    }.bind(this));
                }
            }
        });
        return raceJSON;
    }
}
