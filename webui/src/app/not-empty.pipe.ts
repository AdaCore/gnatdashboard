import { Pipe, PipeTransform } from '@angular/core';

/**
 * Pipe to check whether object is object or not.
 *
 * An object is empty iff it has 0 keys.
 */
@Pipe({ name: 'dshNotEmpty'})
export class NotEmptyPipe implements PipeTransform {
    public transform(obj: { [key: string]: any }, args: any[] = null): boolean {
        return Object.keys(obj || {}).length !== 0;
    }
}
