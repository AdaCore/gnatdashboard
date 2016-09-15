import { Pipe, PipeTransform } from '@angular/core';

/**
 * Extract the list of keys out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="let key of obj | mapKeys">{{ key }}</li>
 */
@Pipe({ name: 'mapKeys'})
export class MapKeys implements PipeTransform {
    transform(obj: { [key: string]: any }, args: any[] = null): string[] {
        return obj ? Object.keys(obj) : [];
    }
}

/**
 * Extract the list of properties out of a JavaScript object.
 *
 * Use with NgFor directive.
 *
 * Example:
 *     <li *ngFor="let value of obj | mapValues">{{ value }}</li>
 */
@Pipe({ name: 'mapValues'})
export class MapValues implements PipeTransform {
    transform(obj: { [key: string]: any }, args: any[] = null): string[] {
        return obj ? Object.keys(obj).map(key => obj[key]) : [];
    }
}

/**
 * Pipe to check whether object is object or not.
 *
 * An object is empty iff it has 0 keys.
 */
@Pipe({ name: 'notEmpty'})
export class NotEmpty implements PipeTransform {
    transform(obj: { [key: string]: any }, args: any[] = null): boolean {
        return Object.keys(obj).length !== 0;
    }
}
