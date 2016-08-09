import { Pipe, PipeTransform } from '@angular/core';

type CountInput = { [key: number]: any } | { [key: string]: any } | any[];

/**
 * Count the number of elements out of a JavaScript object or array.
 *
 * Example:
 *     <div>{{ { 0: 'a', 1: 'b', 2: 'c' } | count }}</div>
 *     <div>{{ [0, 1, 2, 3] | count }}</div>
 */
@Pipe({ name: 'count'})
export class Count implements PipeTransform {
    transform(value: CountInput, args: any[] = null): number {
        return value instanceof Array ?
            value.length : Object.keys(value).length;
    }
}
