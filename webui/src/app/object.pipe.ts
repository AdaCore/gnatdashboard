import { Pipe, PipeTransform } from '@angular/core';

/**
 * Extract the list of keys out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="#key of obj | mapKeys">{{ key }}</li>
 */
@Pipe({ name: 'mapKeys'})
export class MapKeys implements PipeTransform {
    transform(obj: { [key: string]: any }, args: any[] = null): string[] {
        return obj ? Object.keys(obj): [];
    }
}

/**
 * Extract the list of properties out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="#value of obj | mapValues">{{ value }}</li>
 */
@Pipe({ name: 'mapValues'})
export class MapValues implements PipeTransform {
    transform(obj: { [key: string]: any }, args: any[] = null): string[] {
        return obj ? Object.keys(obj).map(key => obj[key]): [];
    }
}
