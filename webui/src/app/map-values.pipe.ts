import { Pipe, PipeTransform } from '@angular/core';

/**
 * Extract the list of properties out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="let value of obj | dshMapValues">{{ value }}</li>
 */
@Pipe({ name: 'dshMapValues', pure: false})
export class MapValuesPipe implements PipeTransform {
    public transform(obj: { [key: string]: any }, args: any[] = null): string[]
    {
        return obj ? Object.keys(obj).map(key => obj[key]) : [];
    }
}
