import { Injectable } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { IFilterIndex, ICodeIndex, IMessageIndex } from 'gnat';
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
  public hideFiles: boolean = false;
  public isReportFetchError: boolean = false;
  public projectName: string;
  public _ui_total_message_count: number = -1;

  private client_host: string = window.location.origin;
  private client_port: number = Number(window.location.port);
  private api_port: number = this.client_port + 1;
  private url: string = this.client_host.replace(String(this.client_port), String(this.api_port)+"/");

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
        this.projectName = this.filter.project;
      }, error => {
        console.log("[Error] get filter : ", error);
        this.gnathub.getFilter().subscribe(
          filter => {
            this.filter = filter;
            this.projectName = filter.project;
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

  private addUserReview(messages) {
    this.message = messages;
    this.message.sources = sortMessageArray(
      this.messageFilter,
      this.messageFilter, messages.sources);

    this.message.sources.forEach(function(source){
      if (source.messages){
        source.messages.forEach(function(message){
          if (this.codepeer_review[message.tool_msg_id]) {
            if (!message.user_review) {
              message.user_review = [];
            }
            message.user_review.push(this.codepeer_review[message.tool_msg_id]);
          }
        }.bind(this));
      }
    }.bind(this));
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
