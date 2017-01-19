import { Pipe, PipeTransform } from '@angular/core';

type CountInput = { [key: number]: any } | { [key: string]: any } | any[];

/**
 * CountPipe the number of elements out of a JavaScript object or array.
 *
 * Example:
 *     <div>{{ { 0: 'a', 1: 'b', 2: 'c' } | dshCount }}</div>
 *     <div>{{ [0, 1, 2, 3] | dshCount }}</div>
 */
@Pipe({ name: 'dshCount'})
export class CountPipe implements PipeTransform {
    public transform(value: CountInput, args: any[] = null): number {
        return value instanceof Array ?
            value.length : Object.keys(value).length;
    }
}
