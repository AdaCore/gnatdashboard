import { Injectable } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { IFilterIndex, ICodeIndex, IMessageIndex, IReviewFilter, IRankingFilter } from 'gnat';
import { sortCodeArray, sortMessageArray } from './utils/sortArray';
import { Http, Response } from '@angular/http';

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
  public filter: IFilterIndex;
  public code: ICodeIndex;
  public message: IMessageIndex;
  public codepeer_review: any;

  public codeFilter = {newSort: 'name', otherSort: 'filename', order: 1};
  public messageFilter = {newSort: 'name', otherSort: 'filename', order: 1};

  public page: string;
  public showFiles: boolean = false;
  public showReviews: boolean = true;
  public isReportFetchError: boolean = false;
  public projectName: string;
  public _ui_total_message_count: number = -1;

  private userReviewFilter: [IReviewFilter];

  private client_host: string = window.location.origin;
  private client_port: number = Number(window.location.port);
  private api_port: number = this.client_port + 1;
  private url: string = this.client_host.replace(String(this.client_port), String(this.api_port)+"/");

  public history = [];
  public codepeer_code = -1;
  public selectedMessage = [];

  constructor( private gnathub: GNAThubService, private http: Http ) {

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
        this.orderRanking();
        this.projectName = this.filter.project;
        this.getCodepeerCode();
      }, error => {
        console.log("[Error] get filter : ", error);
        this.gnathub.getFilter().subscribe(
          filter => {
            this.filter = filter;
            this.projectName = filter.project;
            this.getCodepeerCode();
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
      }, error => {
        console.log("[Error] get code : ", error);
        this.gnathub.getCode().subscribe(
          object => {
            this.code = object;
            this.code.modules = sortCodeArray(
              this.codeFilter,
              this.codeFilter, object.modules);
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
        this.getUserReview(this.message);
      }, error => {
        console.log("[Error] get message : ", error);
        this.gnathub.getMessage().subscribe(
          messages => {
            this.message = messages;
            this.message.sources = sortMessageArray(
              this.messageFilter,
              this.messageFilter, messages.sources);
            //this.getUserReview(object);
          }, error => {
            this.isReportFetchError = true;
          }
        );
      }
    );
  }

  private orderRanking() {
    let order = ['Info', 'Low', 'Medium', 'High', 'Unspecified'];
    let newOrder: [IRankingFilter] ;

    order.forEach(function(status){
      this.filter.ranking.forEach(function(rank){
        if (newOrder && rank.name == status) {
           newOrder.push(rank);
        } else if (rank.name == status) {
          newOrder = [rank];
        }

      })
    }.bind(this));

    this.filter.ranking = newOrder;
  }


  private getCodepeerCode() {
    this.filter.tools.forEach(function(tool){
      if (tool.name == 'codepeer'){
        this.codepeer_code = tool.id;
      }
    }.bind(this));
  }

  private getUserReview(messages) {
    let url = this.url + 'get-review/';
    this.http.get(url + 'codepeer_review.xml')
      .subscribe(
      data => {
        this.codepeer_review = this.gnathub.convertToJson(data);
        this.addUserReview(messages);
      }, error => {
        console.log("[Error] get codepeer_review : ", error);
        this.gnathub.getReview().subscribe(
          object => {
            this.codepeer_review = object;
            this.addUserReview(messages);
          }, error => {
            this.isReportFetchError = true;
          }
        );
      }
    );
  }

  private countUncategorized(filter, total_count) {
    let count = 0;
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

  private putInFilter(status, filter): [IReviewFilter] {
    let stop = false;
    let newIdx = 1;
    if (filter){
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
      let tmp = {
        id : newIdx,
        name : status,
        _message_count : 1
      };
      if (!filter){
        filter = [tmp];
      } else {
        filter.push(tmp);
      }
    }
    return filter;
  }

  private addUserReview(messages) {
    this.message = messages;
    this.message.sources = sortMessageArray(
      this.messageFilter,
      this.messageFilter, messages.sources);

    this.message.sources.forEach(function(source){
      if (source.messages){
        source.messages.forEach(function(message){

          if (this.codepeer_review[message.tool_msg_id]) {
            message.review_history = this.codepeer_review[message.tool_msg_id].review_history;
            message.user_review = this.codepeer_review[message.tool_msg_id].user_review;

            this.userReviewFilter = this.putInFilter(message.user_review.status, this.userReviewFilter);
          }
        }.bind(this));
      }
    }.bind(this));
    if (this.filter){
      this.putInFilter('UNCATEGORIZED', this.userReviewFilter);
      this.countUncategorized(this.userReviewFilter, this.filter._total_message_count);
      this.filter.review_status = this.userReviewFilter;
    }
  }

  public sendUserReview(xml) {
    let url = this.url + "post-review/";

    this.http.post(url, xml)
      .subscribe(data => {
    }, error => {
      console.log("[Error] sendUserReview :", error);
    });
  }

}
