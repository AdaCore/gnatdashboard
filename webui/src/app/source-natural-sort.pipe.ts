import { Pipe, PipeTransform } from '@angular/core';
import * as naturalSort from 'natural-sort';

import { ISource } from 'gnat';

/**
 * Sort the array in natural sort order.
 *
 * @see https://en.wikipedia.org/wiki/Natural_sort_order
 */
@Pipe({ name: 'dshSourceNaturalSort'})
export class SourceNaturalSortPipe implements PipeTransform {
    public transform(sources: { [filename: string]: ISource },
                     args: any[] = null): ISource[]
    {
        return Object.keys(sources)
            .sort(naturalSort({ caseSensitive: true }))
            .map(filename => sources[filename]);
    }
}
