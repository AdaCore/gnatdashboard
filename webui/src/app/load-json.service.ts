import { Observable } from 'rxjs';
import { Injectable } from '@angular/core';
import { map } from 'rxjs/operators';
import {ScriptService} from "./load-script-service";

// this is an external variable that will be progressively filled when
// loading scripts
declare let REPORT: any;

@Injectable({
  providedIn: 'root',
})
export class LoadJsonService {

  constructor(private scriptService: ScriptService) {}

  public getJSON(url: string): Observable<any> {
    const keySplit: string[] = url.split('/');
    const key: string = keySplit[keySplit.length - 1];
    return this.scriptService.loadScript(url).pipe(
      map(
        (_anything: any) =>
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access,@typescript-eslint/no-unsafe-return
          REPORT[key]
      )
    );
  }
}
