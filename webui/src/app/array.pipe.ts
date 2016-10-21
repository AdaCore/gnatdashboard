import { Pipe, PipeTransform } from '@angular/core';
import * as naturalSort from 'natural-sort';

/**
 * Sort the array in natural sort order.
 *
 * @see https://en.wikipedia.org/wiki/Natural_sort_order
 */
@Pipe({ name: 'naturalSort'})
export class ArrayNaturalSortPipe implements PipeTransform {
    transform(arr: string[], args: any[] = null): string[] {
        return arr.sort(naturalSort({ caseSensitive: true }));
    }
}
