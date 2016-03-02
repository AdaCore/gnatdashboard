import { Pipe, PipeTransform } from "angular2/core";

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
    transform(value: { [key: string]: any }, args: any[] = null): string[] {
        return value ? Object.keys(value): [];
    }
}
