import { Pipe, PipeTransform } from '@angular/core';

/**
 * Extract the list of keys out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="let key of obj | dshMapKeys">{{ key }}</li>
 */
@Pipe({ name: 'dshMapKeys', pure: false})
export class MapKeysPipe implements PipeTransform {
    public transform(obj: { [key: string]: any }, args: any[] = null): string[]
    {
        return obj ? Object.keys(obj) : [];
    }
}
