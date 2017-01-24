import { Pipe, PipeTransform } from '@angular/core';
import { ISource } from 'gnat';

import './array/operator/sum';

/**
 * Count the number of messages attached to a given source file.
 */
@Pipe({ name: 'dshMessageCount'})
export class MessageCountPipe implements PipeTransform {
    public transform(source: ISource, args: any[] = null): number {
        if (!source.message_count) {
            return 0;
        }
        return Object.keys(source.message_count).sum(
            toolId => source.message_count[toolId]);
    }
}
