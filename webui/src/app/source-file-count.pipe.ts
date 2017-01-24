import { Pipe, PipeTransform } from '@angular/core';
import { ISourceDir } from 'gnat';

/**
 * Count the number of source files in a given source directory.
 *
 * Example:
 *     <div>{{ sourceDir | dshSourceFileCount }}</div>
 */
@Pipe({ name: 'dshSourceFileCount'})
export class SourceFileCountPipe implements PipeTransform {
    public transform(sourceDir: ISourceDir, args: any[] = null): number {
        return sourceDir.sources.length;
    }
}
