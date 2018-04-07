import { Injectable } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { ActivatedRoute } from '@angular/router';
import { IFilterIndex, ICodeIndex, IMessageIndex } from 'gnat';
import { sortCodeArray, sortMessageArray } from './utils/sortArray';

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

  public codeFilter = {newSort: 'name', otherSort: 'filename', order: 1};
  public messageFilter = {newSort: 'name', otherSort: 'filename', order: 1};

  public page: string;
  public hideFiles: boolean = true;
  public isReportFetchError: boolean = false;
  public projectName: string;
  public _ui_total_message_count: number = -1;

  constructor( private gnathub: GNAThubService ) {
    this.gnathub.getFilter().subscribe(
      filter => {
        this.filter = filter;
        this.projectName = filter.project;
      }, error => {
        this.isReportFetchError = true;
      }
    );
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
    this.gnathub.getMessage().subscribe(
      object => {
        this.message = object;
        this.message.sources = sortMessageArray(
          this.messageFilter,
          this.messageFilter, object.sources);
      }, error => {
        this.isReportFetchError = true;
      }
    );
  }

}
