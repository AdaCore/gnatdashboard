import { Injectable } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { IFilterIndex, ICodeIndex, IMessageIndex, IReviewFilter, IRankingFilter } from 'gnat';
import { sortCodeArray, sortMessageArray } from './utils/sortArray';
import { Http, Response } from '@angular/http';
import { updateFilter } from './utils/refreshFilter';

export type InteralStateType = {
    [key: string]: any
};

@Injectable()
export class AppState {
    public _state: InteralStateType = { };

    // already return a clone of the current state
    public get state() {
        return this._state = this._clone(this._state);
    }

    // never allow mutation
    public set state(value) {
        throw new Error('do not mutate the `.state` directly');
    }

    public get(prop?: any) {
        // use our state getter for the clone
        const state = this.state;
        return state.hasOwnProperty(prop) ? state[prop] : state;
    }

    public set(prop: string, value: any) {
        // internally mutate our state
        return this._state[prop] = value;
    }

    private _clone(object: InteralStateType) {
        // simple object clone
        return JSON.parse(JSON.stringify( object ));
    }
}

@Injectable()
export class SharedReport {
    /* Correspond to the data extracted from the gnathub database*/
    public filter: IFilterIndex;
    public code: ICodeIndex;
    public message: IMessageIndex;

    /* Corresponds to the sorting state of code and message navigation*/
    public codeFilter = {newSort: 'name', otherSort: 'filename', order: 1};
    public messageFilter = {newSort: 'filename', otherSort: 'line', order: 1};

    /* Corresponds to the gloal info needed in the web navigation */
    public page: string;
    public projectName: string;
    public history = {
        message: {},
        reviews: []
    };
    public codepeer_code = -1;
    public selectedMessage = [];
    public showFiles: boolean = false;
    public showCoverage: boolean = false;
    public isReportFetchError: boolean = false;
    public codepeerReviewError: boolean = false;
    public isFilter: boolean = true;
    public _ui_total_message_count: number = -1;

    // TODO : verify these variables
    public showReviews: boolean = true;

    /* Corresponds to the variable needed to initiate filter */
    private codepeer_review: any;
    private activeFilter: boolean = false;
    private userReviewFilter: [IReviewFilter];

    /* Correspond to the information needed to contact the API */
    private client_host: string = window.location.origin;
    private client_port: number = Number(window.location.port);
    private api_port: number = this.client_port + 1;
    private url: string = this.client_host.replace(String(this.client_port), String(this.api_port)+"/");

    constructor( private gnathub: GNAThubService, private http: Http) {

        /*
     * This part is for the connection with the server
     * If server not running or responding, use the old get
     */
        console.log("Designated API Url :", this.url)

        let url = this.url + "json/";
        this.http.get(url + 'filter.json')
            .subscribe(
            data => {
                this.filter = JSON.parse(JSON.parse(data['_body']));
                this.initFilter();
                this.projectName = this.filter.project;
                this.getCodepeerCode();
                if (this.filter.tools == null) {
                    this.isFilter = false;
                }
                this.refreshFilter()
            }, error => {
                console.log("[Error] get filter : ", error);
                this.gnathub.getFilter().subscribe(
                    filter => {
                        this.filter = filter;
                        this.initFilter();
                        this.projectName = filter.project;
                        this.getCodepeerCode();
                        if (this.filter.tools == null) {
                            this.isFilter = false;
                        }
                        this.refreshFilter()
                    }, error => {
                        this.isReportFetchError = true;
                    }
                );
            }
        );

        this.http.get(url + 'code.json')
            .subscribe(
            data => {
                this.code = JSON.parse(JSON.parse(data['_body']));
                this.code.modules = sortCodeArray(
                    this.codeFilter,
                    this.codeFilter, this.code.modules);
                this.refreshFilter()
            }, error => {
                console.log("[Error] get code : ", error);
                this.gnathub.getCode().subscribe(
                    object => {
                        this.code = object;
                        this.code.modules = sortCodeArray(
                            this.codeFilter,
                            this.codeFilter, object.modules);
                        this.refreshFilter()
                    }, error => {
                        this.isReportFetchError = true;
                    }
                );
            }
        );

        this.http.get(url + 'message.json')
            .subscribe(
            data => {
                this.message = JSON.parse(JSON.parse(data['_body']));
                this.getUserReview();
                this.refreshFilter()
            }, error => {
                console.log("[Error] get message : ", error);
                this.gnathub.getMessage().subscribe(
                    messages => {
                        this.message = messages;
                        this.getUserReview();
                        this.refreshFilter()
                    }, error => {
                        this.isReportFetchError = true;
                    }
                );
            }
        );
    }

    private getUserReview() {
        let url = this.url + 'get-review/';
        this.http.get(url + 'codepeer_review.xml')
            .subscribe(
            data => {
                this.codepeer_review = this.gnathub.convertToJson(data);
                this.addUserReview();
            }, error => {
                console.log("[Error] get codepeer_review : ", error);
                this.addUserReview();
                this.codepeerReviewError = true;
            }
        );
    }

    /* This function is called each time a set of data is loaded.
     * It will verify if all the set of data are loaded.
     * If there are, it will launch the global function to refresh the filter properly.
     */
    private refreshFilter() {
        if (this.code && this.filter && this.message && this.filter.review_status){
            updateFilter(this);
            this.messageFilter = {newSort: 'ranking', otherSort: 'countRanking', order: -1};
            this.message.sources = sortMessageArray(
                this.messageFilter,
                this.messageFilter, this.message.sources);
        }
    }

    private initFilter() {
        this.initHistory();
        this.initRanking();
    }

    private initHistory() {
        let unselected = ['Removed'];
        this.unselectFilter(this.filter.properties, unselected);
    }

    private initRanking() {
        let order = ['High', 'Medium', 'Low', 'Info', 'Unspecified'];
        let unselected = ['Info','Low'];

        this.filter.ranking = this.orderFilter(this.filter.ranking, order);
        this.unselectFilter(this.filter.ranking, unselected);
    }

    private orderFilter(myArray: any, orderArray: string[]){
        let newOrder: [any] ;

        orderArray.forEach(function(status){
            if (this.checkArray(myArray, "main-responder.service",
                                "initFilter", "myArray")){
                myArray.forEach(function(rank){
                    if (newOrder && rank.name == status) {
                        newOrder.push(rank);
                    } else if (rank.name == status) {
                        newOrder = [rank];
                    }

                })
            }
        }.bind(this));
        myArray = newOrder;
        return newOrder;
    }

    private unselectFilter(myArray: any, unselectArray: string[]){
        let newFilter: any ;

        if (this.checkArray(myArray, "main-responder.service",
                            "initFilter", "myArray")){
            myArray.forEach(function(rank){

                unselectArray.forEach(function(unselect){
                    if (rank.name == unselect){
                        rank._ui_unselected = true;
                    }
                });

            })
        }
        myArray = newFilter;
    }

    private checkCoverage() {
        if (this.checkArray(this.code.modules, "main-responder.service",
                            "checkCoverage", "code.modules")){
            this.code.modules.forEach(function(project){
                if (project.coverage > 0) {
                    this.showCoverage = true;
                }
                if (this.checkArray( project.source_dirs, "main-responder.service",
                                    "checkCoverage", " project.source_dirs")){
                    project.source_dirs.forEach(function(folder){
                        if (folder.coverage > 0) {
                            this.showCoverage = true;
                        }
                        if (this.checkArray(folder.sources, "main-responder.service",
                                            "checkCoverage", "folder.sources")){
                            folder.sources.forEach(function(source){
                                if (source.coverage > 0) {
                                    this.showCoverage = true;
                                }
                            });
                        }
                    });
                }
            });
        }
    }

    private getCodepeerCode() {
        if (this.checkArray(this.filter.tools, "main-responder.service",
                            "getCodepeerCode", "filter.tools")){
            this.filter.tools.forEach(function(tool){
                if (tool.name == 'codepeer'){
                    this.codepeer_code = tool.id;
                }
            }.bind(this));
        }
    }

    public countUncategorized(filter, total_count) {
        let count = 0;
        if (this.checkArray(filter, "main-responder.service",
                            "countUncategorized", "filter")){
            filter.forEach(function(reviewStatus){
                if (reviewStatus.name != 'UNCATEGORIZED') {
                    count += reviewStatus._message_count;
                }
            });
            count = total_count - count;
            filter.forEach(function(reviewStatus){
                if (reviewStatus.name == 'UNCATEGORIZED') {
                    reviewStatus._message_count = count;
                }
            });
        }
    }

    private isActiveFilter(status): boolean {
        let unselected = ['FALSE_POSITIVE','INTENTIONAL', 'NOT_A_BUG'];
        this.activeFilter =  unselected.indexOf(status) == -1 ? false :  true;

        if (this.checkArray(this.filter.review_status, "main-responder.service",
                            "isActiveFilter", "this.filter.review_status")){
            this.filter.review_status.forEach(function(status, unselected, review, idx){

                if (review.name == status){
                    this.activeFilter = review._ui_unselected;
                }
            }.bind(this, status, unselected));
        }
        return this.activeFilter;
    }

    public putInFilter(review, filter): [IReviewFilter] {
        let stop = false;
        let newIdx = 1;
        let status = review.status;
        let display_name = review.display_name;

        if (this.checkArray(filter, "main-responder.service",
                            "putInFilter", "filter")){
            filter.forEach(function(reviewStatus){
                if (reviewStatus.id >= newIdx){
                    newIdx = reviewStatus.id + 1;
                }
                if (!stop && reviewStatus.name == status){
                    reviewStatus._message_count += 1;
                    stop = true;
                }
            });
        }
        if (!stop) {
            let active = this.isActiveFilter(status);
            let tmp = {
                id : newIdx,
                name : status,
                display_name: display_name,
                _message_count : 1,
                _ui_unselected : active
            };
            if (!filter){
                filter = [tmp];
            } else {
                filter.push(tmp);
            }
        }
        return filter;
    }

    private addUserReview() {
        if (this.checkArray(this.message.sources, "main-responder.service",
                            "addUserReview", "message.sources")){
            this.message.sources.forEach(function(source){
                source.expand = false;
                if (source.messages){
                    source.messages.forEach(function(message){

                        if (this.codepeer_review[message.tool_msg_id]) {
                            message.review_history = this.codepeer_review[message.tool_msg_id].review_history;
                            message.user_review = this.codepeer_review[message.tool_msg_id].user_review;
                            this.userReviewFilter = this.putInFilter(message.user_review, this.userReviewFilter);
                        }
                    }.bind(this));
                }
            }.bind(this));
        }
        let tmp_review = {
            status: "UNCATEGORIZED",
            display_name: "Uncategorized"
        };
        this.userReviewFilter = this.putInFilter(tmp_review ,this.userReviewFilter);
        this.countUncategorized(this.userReviewFilter, this.filter._total_message_count);
        this.filter.review_status = this.userReviewFilter;
        this.refreshFilter();
    }

    public sendUserReview(xml) {
        let url = this.url + "post-review/";

        this.http.post(url, xml)
            .subscribe(data => {
        }, error => {
            console.log("[Error] sendUserReview :", error);
        });
    }

    public checkArray(array, file, func_name, name): boolean {
        if (!array){
            console.log("[Warning] " + file + ":" + func_name +" : " + name + " doesn´t exist.");
            return false;
        } else if (array == null){
            console.log("[Warning] " + file + ":" + func_name +" : " + name + " is null.");
            return false;
        } else if (array.length == 0) {
            console.log("[Warning] " + file + ":" + func_name +" : " + name + " is empty.");
            return false;
        } else {
            return true;
        }
    }
}
