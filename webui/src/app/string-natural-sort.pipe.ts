import { Pipe, PipeTransform } from '@angular/core';
import * as naturalSort from 'natural-sort';

/**
 * Sort the array in natural sort order.
 *
 * @see https://en.wikipedia.org/wiki/Natural_sort_order
 */
@Pipe({ name: 'dshStringNaturalSort'})
export class StringNaturalSortPipe implements PipeTransform {
    public transform(arr: string[], args: any[] = null): string[] {
        return arr.sort(naturalSort({ caseSensitive: true }));
    }
}
