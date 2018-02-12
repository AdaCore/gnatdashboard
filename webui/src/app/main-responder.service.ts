import { Injectable } from '@angular/core';
import { GNAThubService } from './gnathub.service';
import { ActivatedRoute } from '@angular/router';
import { IReportIndex } from 'gnat';
import { sortMapInArray } from './project-explorer/project-sort.component';

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
  public report: IReportIndex;
  public modulesFilter = {newSort: 'name', otherSort: '', order: -1};
  public page: string;
  public hideFiles:boolean = false;
  public isReportFetchError:boolean = false;

  constructor( private gnathub: GNAThubService,
                private route: ActivatedRoute) {
    this.gnathub.getReport().subscribe(
      report => {
        this.report = report;
        this.report.showed_modules = sortMapInArray(
          {newSort: 'name', otherSort: ''},
          this.modulesFilter, report.modules);
        console.log("SharedReport.report : ", this.report); //will be erased before merge
      }, error => {
        this.isReportFetchError = true;
      }
    );
  }

}
