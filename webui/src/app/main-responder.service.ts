import { Injectable, Inject } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import {
    IFilterIndex, ICodeIndex,  IMessageIndex, IMessage,
    IModule, ISourceDir, ISource, IReviewFilter,
    IRankingFilter, ISort, ITool, IReviewUser, ISourceNav
} from 'gnat';
import { sortCodeArray, sortMessageArray } from './utils/sortArray';
import { createDisplayName } from './utils/createDisplayName';
import { Http, Response } from '@angular/http';
import { updateFilter } from './utils/refreshFilter';
import {
    storeFilterItem, getFilterItem, getStoredFilter,
    getStoredMessageSort, getStoredProjectSort
} from './utils/dataStorage';
import { DOCUMENT } from '@angular/common';

export type InteralStateType = {
    [key: string]: any
};

@Injectable()
export class AppState {
    public _state: InteralStateType = {};

    // already return a clone of the current state
    public get state(): InteralStateType {
        return this._state = this._clone(this._state);
    }

    // never allow mutation
    public set state(value: InteralStateType) {
        throw new Error('do not mutate the `.state` directly');
    }

    public get(prop?: any): InteralStateType {
        // use our state getter for the clone
        const state: InteralStateType = this.state;
        return state.hasOwnProperty(prop) ? state[prop] : state;
    }

    public set(prop: string, value: any): void {
        // internally mutate our state
        return this._state[prop] = value;
    }

    private _clone(object: InteralStateType): any {
        // simple object clone
        return JSON.parse(JSON.stringify(object));
    }
}

@Injectable()
export class SharedReport {
    public isOnline: boolean = false;

    /* Correspond to the data extracted from the gnathub database*/
    public filter: IFilterIndex;
    public code: ICodeIndex;
    public message: IMessageIndex;
    public globalReviewStatus: any[] = [];
    private isInitLocalStorage: boolean = false;

    /* Correspond to the data extracted from the codepeer tool*/
    public isCodepeer: boolean = false;
    public codepeerRunInfo: any;
    public codepeerCurrentRun: number = 0;
    public codepeerCode: number = -1;
    public codepeerReviewError: boolean = false;
    private codepeerReview: any;
    public codepeerHistory: any;
    public raceCondition: any;
    public showRace: boolean = false;
    public isAnnotations: boolean = false;
    public showAnnotations: boolean = true;

    /* Corresponds to the sorting state of code and message navigation*/
    public projectSort: ISort = getStoredProjectSort();
    public messageSort: ISort = getStoredMessageSort();

    /* Corresponds to the gloal info needed in the web navigation */
    public page: string;
    public projectName: string;
    public history: any = {
        message: {},
        reviews: []
    };

    public selectedMessage: IMessage[] = [];
    public showFiles: boolean = false;
    public showCoverage: boolean = false;
    public isReportFetchError: boolean = false;
    public errorToShow: string[] = [];

    public isFilter: boolean = true;
    public totalMessageCount: number = -1;

    // TODO : verify these variables
    public showReviews: boolean = true;

    /* Corresponds to the variable needed to initiate filter */
    private activeFilter: boolean = false;
    private userReviewFilter: [IReviewFilter];

    /* Correspond to the information needed to contact the API */
    private serverHost: string = window.location.origin;
    public url: string = this.serverHost + '/';

    constructor(@Inject(DOCUMENT) private document: Document,
                private gnathub: GNAThubService,
                private http: Http) {

        /*
         * This part is for the connection with the server
         * If server not running or responding, use the old get
         */
        console.log('Designated API Url :', this.url);
        this.initApp();
    }

    private initApp(): void {
        this.initLocalStorage();
        this.getCustomReview();
        this.isServerOnline();
        this.isCodepeerPassed();
    }

    private initLocalStorage(): void {
        if (localStorage.length === 0) {
            storeFilterItem('Low', true);
            storeFilterItem('Info', true);
            storeFilterItem('Removed', true);
            this.isInitLocalStorage = true;
        }
    }

    private initCustomLocalStorage(customStatusArray: any[]): void {
        if (this.isInitLocalStorage) {
             customStatusArray.forEach(function(status: any): void {
                storeFilterItem(status.value, true);
             });
        }
    }

    private isServerOnline(): void {
        this.http.get(this.url + 'online-server')
            .subscribe(
                data => {
                    if (data.status === 200) {
                        console.log('[Success] The server is online.');
                        this.isOnline = true;
                        this.getFilterOnline();
                        this.getCodeOnline();
                        this.getMessageOnline();
                    } else {
                        this.serverOffline();
                    }
                }, error => {
                    console.log('[Warning] main-responder.service:isServerOnline: ', error);
                    this.serverOffline();
                }
            );
    }

    private serverOffline(): void {
        console.log('[Warning] The server is offline');
        this.isOnline = false;
        this.getFilterOffline();
        this.getCodeOffline();
        this.getMessageOffline();
    }

    private CodepeerVersionError(): void {
        this.errorToShow.push('You are running an old html interface with new codepeer results.');
        this.errorToShow.push('This could trigger some error in the web interface.');
        this.errorToShow.push('To ensure the best experience,');
        this.errorToShow.push(' please re-run codepeer with the -html or -html-only flag');
        this.triggerError();
    }

    private isCodepeerPassed(): void {
        this.http.get(this.url + 'codepeer-passed')
            .subscribe(
                data => {
                    if (data.status !== 204) {
                        this.isCodepeer = true;
                        // If version of WebUI doesn't match version of Codepeer
                        if (data.status === 202) {
                            this.CodepeerVersionError();
                            this.isCodepeer = false;
                        }
                        this.getUserReview();
                        this.getRaceCondition();
                        this.getCodepeerRunInfo();
                    }
                }, error => {
                    console.log('[Error] is codepeer passed : ', error);
                    this.isCodepeer = false;
                }
            );
    }

    private getFilterOnline(): void {
        this.http.get(this.url + 'json/filter.json')
            .subscribe(
                data => {
                    this.filter = JSON.parse(data['_body']);
                    this.initFilter();
                    this.projectName = this.filter.project;
                    this.getCodepeerCode();
                    if (this.filter.tools == null) {
                        this.isFilter = false;
                    }
                    this.refreshFilter();
                }, error => {
                    console.log('[Error] get filter : ', error);
                    this.getFilterOffline();
                }
            );
    }
    private getFilterOffline(): void {
        this.gnathub.getFilter().subscribe(
            filter => {
                this.filter = filter;
                this.initFilter();
                this.projectName = filter.project;
                this.getCodepeerCode();
                if (this.filter.tools == null) {
                    this.isFilter = false;
                }
                this.refreshFilter();
            }, error => {
                this.isReportFetchError = true;
            }
        );
    }

    private getCodeOnline(): void {
        this.http.get(this.url + 'json/code.json')
            .subscribe(
                data => {
                    this.code = JSON.parse(data['_body']);
                    this.code.modules = sortCodeArray(
                        this.projectSort,
                        this.projectSort, this.code.modules);
                    this.refreshFilter();
                }, error => {
                    console.log('[Error] get code : ', error);
                    this.getCodeOffline();
                }
            );
    }
    private getCodeOffline(): void {
        this.gnathub.getCode().subscribe(
            object => {
                this.code = object;
                this.code.modules = sortCodeArray(
                    this.projectSort,
                    this.projectSort, object.modules);
                this.refreshFilter();
            }, error => {
                this.isReportFetchError = true;
            }
        );
    }

    private getMessageOnline(): void {
        this.http.get(this.url + 'json/message.json')
            .subscribe(
                data => {
                    this.message = JSON.parse(data['_body']);
                    this.refreshFilter();
                }, error => {
                    console.log('[Error] get message : ', error);
                    this.getMessageOffline();
                }
            );
    }

    private getMessageOffline(): void {
        this.gnathub.getMessage().subscribe(
            messages => {
                this.message = messages;
                if (this.isCodepeer) {
                    this.getUserReview();
                }
                this.refreshFilter();
            }, error => {
                this.isReportFetchError = true;
            }
        );
    }

    private sortReviewStatus(array: string[], myKind: string, priority: number): any {
        let tmpArray: any[] = [];
        array.sort((a, b) => (a > b ? -1 : 1));
        array.forEach(function(status: string, index: number): void {
            tmpArray.push({
                display_name: createDisplayName(status),
                value: status,
                kind: myKind,
                sortingPriority: priority
            });
            priority += 1;
        }.bind(this));
        return tmpArray;
    }

    private handleReviewStatus(data: any): void {
        let defaultReviewStatus: any = {
            BUG: ['BUG'],
            PENDING: ['PENDING'],
            NOT_A_BUG: ['NOT_A_BUG', 'FALSE_POSITIVE', 'INTENTIONNAL']
        };
        let minPriority: number = 1;
        defaultReviewStatus.NOT_A_BUG = this.sortReviewStatus(defaultReviewStatus
                                                              .NOT_A_BUG.concat(data.NOT_A_BUG),
                                                              'NOT_A_BUG', minPriority);
        this.initCustomLocalStorage(defaultReviewStatus.NOT_A_BUG);
        minPriority += defaultReviewStatus.NOT_A_BUG.length;
        defaultReviewStatus.PENDING = this.sortReviewStatus(defaultReviewStatus
                                                            .PENDING.concat(data.PENDING),
                                                            'PENDING', minPriority);
        minPriority += defaultReviewStatus.PENDING.length;
        defaultReviewStatus.BUG = this.sortReviewStatus(defaultReviewStatus
                                                        .BUG.concat(data.BUG),
                                                        'BUG', minPriority);
        this.globalReviewStatus = defaultReviewStatus.NOT_A_BUG
                                                     .concat(defaultReviewStatus.PENDING)
                                                     .concat(defaultReviewStatus.BUG);
    }

    private getCustomReview(): void {
        this.http.get(this.url + 'json/custom_status.json').subscribe(
                data => {
                    this.handleReviewStatus(JSON.parse(data['_body']));
                }, error => {
                    console.log('[Error] get custom review status : ', error);
                    this.isReportFetchError = true;
                }
        );
    }

    private formatDate(date: string): string {
        let d: Date = new Date(date);
        let month: string = '' + (d.getMonth() + 1);
        let day: string = '' + d.getDate();
        let year: number = d.getFullYear();

        if (month.length < 2){
            month = '0' + month;
        }
        if (day.length < 2){
            day = '0' + day;
        }

        return [year, month, day].join('-');
    }

    private buildCodepeerHistory(history: any[]): any[] {
        let newHistory: any[] = [];
        history.forEach(function (run: any): void {
            let tmp: any = {
                date: run.date,
                format_date: this.formatDate(run.date),
                id: run.inspection_id,
                name: run.inspection_id,
                series: [
                    {
                        name: 'high',
                        value: run.high
                    },
                    {
                        name: 'medium',
                        value: run.medium
                    },
                    {
                        name: 'low',
                        value: run.low
                    }
                ]
            };
            newHistory.push(tmp);
        }.bind(this));
        return newHistory;
    }

    private getCodepeerRunInfo(): void {
        this.gnathub.getCodepeerRun().subscribe(
            runInfo => {
                this.codepeerHistory = this.buildCodepeerHistory(runInfo.history);
                this.codepeerCurrentRun = runInfo.current_run_number;

                let tmp: any[] = [];
                tmp.push({
                    name: 'date',
                    value: runInfo.date
                });
                tmp.push({
                    name: 'codepeer version',
                    value: runInfo.codepeer_version
                });
                tmp.push({
                    name: 'host',
                    value: runInfo.host
                });
                tmp.push({
                    name: 'command line',
                    value: runInfo.command_line
                });
                tmp.push({
                    name: 'codepeer switches ',
                    value: runInfo.codepeer_switches
                });
                tmp.push({
                    name: 'base run number',
                    value: runInfo.base_run_number
                });
                tmp.push({
                    name: 'current run number',
                    value: runInfo.current_run_number
                });
                let tempValue: string = '';
                runInfo.excluded_files.forEach(function (file: string): void {
                    tempValue += file + '';
                });
                tmp.push({
                    name: 'excluded files',
                    value: tempValue
                });

                this.codepeerRunInfo = tmp;
            }, error => {
                this.isReportFetchError = true;
                console.log('[Error] get codepeer_run : ', error);
            }
        );
    }

    private getUserReview(): void {
        if (this.isCodepeer) {
        let url: string = this.url + 'get-review/';
        this.http.get(url + 'codepeer_review.xml')
            .subscribe(
                data => {
                    this.codepeerReview = this.gnathub.convertToJson(data);
                    this.addUserReview();
                }, error => {
                    console.log('[Error] get codepeerReview : ', error);
                    this.addUserReview();
                    this.codepeerReviewError = true;
                }
            );
        }
    }

    public refreshUserReview(): void {
        this.filter.review_status = undefined;
        this.userReviewFilter = undefined;
        this.getUserReview();
    }

    private getRaceCondition(): void {
        if (this.isCodepeer) {
        let url: string = this.url + 'get-race-condition';
        this.http.get(url)
            .subscribe(
                data => {
                    this.raceCondition = this.gnathub.raceToJson(data['_body']);
                    this.showRace = Object.keys(this.raceCondition).length > 0;
                }, error => {
                    this.isReportFetchError = true;
                    console.log('[Error] get getRaceCondition : ', error);
                }
            );
        }
    }

    /* This function is called each time a set of data is loaded.
     * It will verify if all the set of data are loaded.
     * If there are, it will launch the global function to refresh
     * the filter properly.
     */
    private refreshFilter(): void {

        if (this.code && this.filter && this.message && this.filter.review_status) {
            if (localStorage.length > 0) {
                getStoredFilter(this.filter);
            }
            updateFilter(this);
            this.message.sources = sortMessageArray(
                this.messageSort,
                this.messageSort, this.message.sources);
            console.log('this.code', this.code);
            console.log('this.filter', this.filter);
            console.log('this.message', this.message);
        }
    }

    private initFilter(): void {
        this.initRanking();
    }

    private initRanking(): void {
        let order: string[] = ['High', 'Medium', 'Low', 'Info', 'Unspecified'];
        this.filter.ranking = this.orderFilter(this.filter.ranking, order);
    }

    private orderFilter(myArray: any, orderArray: string[]): [any] {
        let newOrder: [any];

        orderArray.forEach(function (status: string): void {
            if (this.checkArray(myArray, 'main-responder.service',
                    'orderFilter', 'myArray')) {
                myArray.forEach(function (rank: any): void {
                    if (newOrder && rank.name === status) {
                        newOrder.push(rank);
                    } else if (rank.name === status) {
                        newOrder = [rank];
                    }
                });
            }
        }.bind(this));
        myArray = newOrder;
        return newOrder;
    }

    private checkCoverage(): void {
        if (this.checkArray(this.code.modules, 'main-responder.service',
                'checkCoverage', 'code.modules')) {
            this.code.modules.forEach(function (project: IModule): void {
                if (project.coverage > 0) {
                    this.showCoverage = true;
                }
                if (this.checkArray(project.source_dirs, 'main-responder.service',
                        'checkCoverage', ' project.source_dirs')) {
                    project.source_dirs.forEach(function (folder: ISourceDir): void {
                        if (folder.coverage > 0) {
                            this.showCoverage = true;
                        }
                        if (this.checkArray(folder.sources, 'main-responder.service',
                                'checkCoverage', 'folder.sources')) {
                            folder.sources.forEach(function (source: ISource): void {
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

    private getCodepeerCode(): void {
        if (this.checkArray(this.filter.tools, 'main-responder.service',
                'getCodepeerCode', 'filter.tools')) {
            this.filter.tools.forEach(function (tool: ITool): void {
                if (tool.name === 'codepeer') {
                    this.codepeerCode = tool.id;
                }
            }.bind(this));
        }
    }

    public countUncategorized(filter: [IReviewFilter], totalCount: number): number {
        let count: number = 0;
        if (this.checkArray(filter, 'main-responder.service',
                'countUncategorized', 'filter')) {
            filter.forEach(function (reviewStatus: IReviewFilter): void {
                if (reviewStatus.name !== 'UNCATEGORIZED') {
                    count += reviewStatus._message_count;
                }
            });
            count = totalCount - count;
            filter.forEach(function (reviewStatus: IReviewFilter): void {
                if (reviewStatus.name === 'UNCATEGORIZED') {
                    reviewStatus._message_count = count;
                }
            });
        }
        return count;
    }

    public putInFilter(review: any, filter: [IReviewFilter]): [IReviewFilter] {
        let stop: boolean = false;
        let newIdx: number = 1;
        let status: string = review.status;
        let displayName: string = review.display_name;
        let statusKind: string = review.status_kind;

        if (this.checkArray(filter, 'main-responder.service',
                'putInFilter', 'filter')) {
            filter.forEach(function (reviewStatus: IReviewFilter): void {
                if (reviewStatus.id >= newIdx) {
                    newIdx = reviewStatus.id + 1;
                }
                if (!stop && reviewStatus.name.toUpperCase() === status.toUpperCase()) {
                    reviewStatus._message_count += 1;
                    stop = true;
                }
            });
        }
        if (!stop) {
            let active: boolean = getFilterItem(displayName);
            let tmp: IReviewFilter = {
                id: newIdx,
                name: status,
                kind: statusKind,
                display_name: displayName,
                _message_count: 1,
                _ui_unselected: active
            };
            if (!filter) {
                filter = [tmp];
            } else {
                filter.push(tmp);
            }
        }
        return filter;
    }

    private getStatusPriority(status: string): number {
        let tmpPriority: number = 0;

        this.globalReviewStatus.forEach(function (reviewStatus: any): void {
            if (reviewStatus.display_name.toUpperCase() === status.toUpperCase()){
                tmpPriority = reviewStatus.sortingPriority;
            }
        });
        if (tmpPriority === 0){
            console.info('[WARNING] Review status (' + status +
                         ') not referenced. Put on priority 0 by default');
        }
        return tmpPriority;
    }

    private addUserReview(): void {
        if (this.checkArray(this.message.sources, 'main-responder.service',
                'addUserReview', 'message.sources')) {
            this.message.sources.forEach(function (source: ISourceNav): void {
                source.expand = false;
                if (source.messages) {
                    source.messages.forEach(function (message: IMessage): void {
                        if (this.codepeerReview[message.tool_msg_id]) {
                            let statusPriority: number = this.getStatusPriority(
                                                         this.codepeerReview[message.tool_msg_id]
                                                         .user_review.display_name);
                            this.codepeerReview[message.tool_msg_id]
                                .user_review.status_priority = statusPriority;
                            message.review_history = this.codepeerReview[message.tool_msg_id]
                                                         .review_history;
                            message.user_review = this.codepeerReview[message.tool_msg_id]
                                                      .user_review;
                            message.status_priority = message.user_review
                                                             .status_priority;
                            this.userReviewFilter = this.putInFilter(message.user_review,
                                                                     this.userReviewFilter);
                        }
                    }.bind(this));
                }
            }.bind(this));
        }
        let count: number = this.countUncategorized(this.userReviewFilter,
                                                    this.filter._total_message_count);
        if (count > 0 || this.userReviewFilter === undefined) {
            let tmpReview: any = {
                status: 'UNCATEGORIZED',
                display_name: 'Uncategorized'
            };
            this.userReviewFilter = this.putInFilter(tmpReview, this.userReviewFilter);
            this.countUncategorized(this.userReviewFilter, this.filter._total_message_count);
        }
        this.filter.review_status = this.userReviewFilter;
        this.refreshFilter();
    }

    private triggerError(): void {
        let elem: HTMLElement = this.document.getElementById('ErrorButton');
        elem.click();
    }

    public verifyServerStatus(): void {
        this.http.get(this.url + 'online-server')
            .subscribe(
                data => {
                    if (data.status === 200) {
                        this.errorToShow.push('Please contact support.');
                    } else {
                        this.errorToShow.push("The server doesn't seem to be running.");
                        this.isOnline = false;
                    }
                    this.triggerError();
                }, error => {
                    console.warn('[Warning] main-responder.service:isServerOnline: ', error);
                    this.errorToShow.push("The server doesn't seem to be running.");
                    this.isOnline = false;
                    this.triggerError();
                }
            );
    }

    public checkArray(array: any[], file: string,
                      functionName: string, name: string): boolean {
        if (!array) {
            console.log('[Warning] ' + file + ':' + functionName + ' : '
                        + name + ' doesn´t exist.');
            return false;
        } else if (array == null) {
            console.log('[Warning] ' + file + ':' + functionName + ' : ' + name + ' is null.');
            return false;
        } else if (array.length === 0) {
            console.log('[Warning] ' + file + ':' + functionName + ' : ' + name + ' is empty.');
            return false;
        } else {
            return true;
        }
    }
}
