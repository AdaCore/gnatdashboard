import { Injectable, Inject } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { IFilterIndex, ICodeIndex, IMessageIndex, IReviewFilter, IRankingFilter } from 'gnat';
import { sortCodeArray, sortMessageArray } from './utils/sortArray';
import { Http, Response } from '@angular/http';
import { updateFilter } from './utils/refreshFilter';
import { storeFilterItem, getFilterItem,
         getStoredFilter, getStoredMessageSort,
         getStoredProjectSort } from './utils/dataStorage';
import { DOCUMENT } from '@angular/common';

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
    public isOnline: boolean = false;

    /* Correspond to the data extracted from the gnathub database*/
    public filter: IFilterIndex;
    public code: ICodeIndex;
    public message: IMessageIndex;

    /* Correspond to the data extracted from the codepeer tool*/
    public isCodepeer: boolean = false;
    public codepeerRunInfo: any;
    public codepeerCurrentRun: number = 0;
    public codepeer_code = -1;
    public codepeerReviewError: boolean = false;
    private codepeer_review: any;
    public codepeer_history: any;

    /* Corresponds to the sorting state of code and message navigation*/
    public projectSort = getStoredProjectSort();
    public messageSort = getStoredMessageSort();

    /* Corresponds to the gloal info needed in the web navigation */
    public page: string;
    public projectName: string;
    public history = {
        message: {},
        reviews: []
    };

    public selectedMessage = [];
    public showFiles: boolean = false;
    public showCoverage: boolean = false;
    public isReportFetchError: boolean = false;
    public errorToShow: string[] = [];

    public isFilter: boolean = true;
    public _ui_total_message_count: number = -1;

    // TODO : verify these variables
    public showReviews: boolean = true;

    /* Corresponds to the variable needed to initiate filter */
    private activeFilter: boolean = false;
    private userReviewFilter: [IReviewFilter];

    /* Correspond to the information needed to contact the API */
    private client_host: string = window.location.origin;
    private client_port: number = Number(window.location.port);
    private api_port: number = this.client_port + 1;
    public url: string = this.client_host.replace(String(this.client_port), String(this.api_port)+"/");



    constructor( @Inject(DOCUMENT) private document: Document,
                  private gnathub: GNAThubService,
                  private http: Http) {

        /*
     * This part is for the connection with the server
     * If server not running or responding, use the old get
     */
        console.log("Designated API Url :", this.url)
        this.initApp();
    }

    private initApp() {
        this.initLocalStorage();
        this.isServerOnline();
        this.isCodepeerPassed();
        this.getCodepeerRunInfo();
    }

    private initLocalStorage() {
        if (localStorage.length == 0){
            storeFilterItem('Low', true);
            storeFilterItem('Info', true);
            storeFilterItem('Removed', true);
            storeFilterItem('False_Positive', true);
            storeFilterItem('Intentional', true);
            storeFilterItem('Not_A_Bug', true);
        }
    }

    private isServerOnline() {
        this.http.get(this.url + 'online-server')
            .subscribe(
            data => {
                if (data.status == 200) {
                    console.log("[Success] The server is online.");
                    this.isOnline = true;
                    this.getFilterOnline()
                    this.getCodeOnline()
                    this.getMessageOnline()
                } else {
                    this.serverOffline();
                }
            }, error => {
                console.log("[Warning] main-responder.service:isServerOnline: ", error);
                this.serverOffline();
            }
        );
    }

    private serverOffline() {
        console.log("[Warning] The server is offline");
        this.isOnline = false;
        this.getFilterOffline()
        this.getCodeOffline()
        this.getMessageOffline()
    }

    private isCodepeerPassed() {
        this.http.get(this.url + 'codepeer-passed')
            .subscribe(
            data => {
                if (data.status == 200) {
                    this.isCodepeer = true;
                    this.getUserReview();
                }
            }, error => {
                console.log("[Error] is codepeer passed : ", error);
                this.isCodepeer = false;
            }
        );
    }

    private getFilterOnline(){
        this.http.get(this.url + 'json/filter.json')
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
                this.getFilterOffline();
            }
        );
    }
    private getFilterOffline(){
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

    private getCodeOnline() {
        this.http.get(this.url + 'json/code.json')
            .subscribe(
            data => {
                this.code = JSON.parse(JSON.parse(data['_body']));
                this.code.modules = sortCodeArray(
                    this.projectSort,
                    this.projectSort, this.code.modules);
                this.refreshFilter()
            }, error => {
                console.log("[Error] get code : ", error);
                this.getCodeOffline();
            }
        );
    }
    private getCodeOffline(){
        this.gnathub.getCode().subscribe(
            object => {
                this.code = object;
                this.code.modules = sortCodeArray(
                    this.projectSort,
                    this.projectSort, object.modules);
                this.refreshFilter()
            }, error => {
                this.isReportFetchError = true;
            }
        );
    }

    private getMessageOnline() {
        this.http.get(this.url + 'json/message.json')
            .subscribe(
            data => {
                this.message = JSON.parse(JSON.parse(data['_body']));
                this.isCodepeerPassed();
                this.refreshFilter()
            }, error => {
                console.log("[Error] get message : ", error);
                this.getMessageOffline();
            }
        );
    }
    private getMessageOffline() {
        this.gnathub.getMessage().subscribe(
            messages => {
                this.message = messages;
                if (this.isCodepeer){
                    this.getUserReview();
                }
                this.refreshFilter()
            }, error => {
                this.isReportFetchError = true;
            }
        );
    }

    private formatDate(date) {
        var d = new Date(date),
            month = '' + (d.getMonth() + 1),
            day = '' + d.getDate(),
            year = d.getFullYear();

        if (month.length < 2)
            month = '0' + month;
        if (day.length < 2)
            day = '0' + day;

        return [year, month, day].join('-');
    }

    private buildCodepeerHistory(history){
        let new_history: any[] = [];
        history.forEach(function(run){
            var tmp = {
                "date": run.date,
                "format_date": this.formatDate(run.date),
                "id": run.inspection_id,
                "name": run.inspection_id,
                "series": [
                    {
                        "name": "high",
                        "value": run.high
                    },
                    {
                        "name": "medium",
                        "value": run.medium
                    },
                    {
                        "name": "low",
                        "value": run.low
                    }
                ]
            };
            new_history.push(tmp);
        }.bind(this));
        return new_history;
    }

    private getCodepeerRunInfo() {
        this.gnathub.getCodepeerRun().subscribe(
            run_info => {
                this.codepeer_history = this.buildCodepeerHistory(run_info.history);
                this.codepeerCurrentRun = run_info.current_run_number;

                var tmp = [];
                tmp.push({
                    name: 'date',
                    value: run_info.date
                });
                tmp.push({
                    name: 'codepeer version',
                    value: run_info.codepeer_version
                });
                tmp.push({
                    name: 'host',
                    value: run_info.host
                });
                tmp.push({
                    name: 'command line',
                    value: run_info.command_line
                });
                tmp.push({
                    name: 'codepeer switches ',
                    value: run_info.codepeer_switches
                });
                tmp.push({
                    name: 'base run number',
                    value: run_info.base_run_number
                });
                tmp.push({
                    name: 'current run number',
                    value: run_info.current_run_number
                });
                let temp_value = "";
                run_info.excluded_files.forEach(function(file) {
                    temp_value += file + "";
                });
                tmp.push({
                    name: 'excluded files',
                    value: temp_value
                });

                this.codepeerRunInfo = tmp;
            }, error => {
                this.isReportFetchError = true;
                console.log("[Error] get codepeer_run : ", error);
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

    public refreshUserReview() {
        this.filter.review_status = undefined;
        this.userReviewFilter = undefined;
        this.getUserReview();
    }

    /* This function is called each time a set of data is loaded.
     * It will verify if all the set of data are loaded.
     * If there are, it will launch the global function to refresh the filter properly.
     */
    private refreshFilter() {
        if (this.code && this.filter && this.message && this.filter.review_status){
            if (localStorage.length > 0){
                getStoredFilter(this.filter);
            }
            updateFilter(this);
            this.message.sources = sortMessageArray(
                this.messageSort,
                this.messageSort, this.message.sources);
            console.log("this.code", this.code);
            console.log("this.filter", this.filter);
            console.log("this.message", this.message);
        }
    }

    private initFilter() {
        this.initRanking();
    }

    private initRanking() {
        let order = ['High', 'Medium', 'Low', 'Info', 'Unspecified'];

        this.filter.ranking = this.orderFilter(this.filter.ranking, order);
    }

    private orderFilter(myArray: any, orderArray: string[]){
        let newOrder: [any] ;

        orderArray.forEach(function(status){
            if (this.checkArray(myArray, "main-responder.service",
                                "orderFilter", "myArray")){
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
        return count;
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
            let active = getFilterItem(display_name);
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
                            message.status_type = message.user_review.status_type;
                            this.userReviewFilter = this.putInFilter(message.user_review, this.userReviewFilter);
                        }
                    }.bind(this));
                }
            }.bind(this));
        }
        let count = this.countUncategorized(this.userReviewFilter, this.filter._total_message_count);
        if (count > 0 || this.userReviewFilter == undefined){
            let tmp_review = {
                status: "UNCATEGORIZED",
                display_name: "Uncategorized"
            };
            this.userReviewFilter = this.putInFilter(tmp_review ,this.userReviewFilter);
            this.countUncategorized(this.userReviewFilter, this.filter._total_message_count);
        }
        this.filter.review_status = this.userReviewFilter;
        this.refreshFilter();
    }


    private triggerError() {
        let elem = this.document.getElementById('ErrorButton');
        elem.click();
    }

    public verifyServerStatus() {
        this.http.get(this.url + 'online-server')
            .subscribe(
            data => {
                if (data.status == 200) {
                    this.errorToShow.push("Please contact support.");
                } else {
                    this.errorToShow.push("The server doesn't seem to be running.");
                    this.isOnline = false;
                }
                this.triggerError();
            }, error => {
                console.warn("[Warning] main-responder.service:isServerOnline: ", error);
                this.errorToShow.push("The server doesn't seem to be running.");
                this.isOnline = false;
                this.triggerError();
            }
        );
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
