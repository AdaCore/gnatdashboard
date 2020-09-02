import { Pipe, PipeTransform} from '@angular/core';

@Pipe({ name: 'values',  pure: true })
export class ValuesPipe implements PipeTransform {
    public transform(value: any, args: any[] = null): any {
        return Object.keys(value).map((key: any) => value[key]);
  }
}

@Pipe({ name: 'keys',  pure: true })
export class KeysPipe implements PipeTransform {
    public transform(value: any, args: any[] = null): any {
        return Object.keys(value);
    }
}
