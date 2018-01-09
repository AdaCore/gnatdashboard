import {
    Component,
    Input,
    OnChanges
} from '@angular/core';
import { IModule, ISourceDir } from 'gnat';
import * as naturalSort from 'natural-sort';
import { sortMapInArray, sortBy } from './project-sort.component';

@Component({
    selector: 'project-source-list',
    templateUrl: 'project-source-list.component.html',
    styleUrls: [ 'project-source-list.component.scss' ]
})
export class ProjectSourceListComponent implements OnChanges {
    @Input() public project: IModule;
    @Input() public directory: string;
    @Input() public change: number;

    public currentSourceDir: ISourceDir;
    public filesFilter = {newSort: 'filename', otherSort: '', order: -1};
    public pathsFilter = {newSort: 'path', otherSort: '', order: -1};
    public ngOnChanges() {
        if (this.directory) {
            this.currentSourceDir = this.project.source_dirs[this.directory];
            return;
        }
        const orderedPath = Object
        .keys(this.project.source_dirs)
        .sort(naturalSort({ caseSensitive: true }));
        if (orderedPath.length) {
            this.currentSourceDir = this.project.source_dirs[orderedPath[0]];
        } else {
            this.currentSourceDir = null;
        }
        this.sortFiles(this.filesFilter.newSort,
                       this.filesFilter.otherSort, this.filesFilter.order);
        this.sortPath(this.pathsFilter.newSort,
                       this.pathsFilter.otherSort, this.pathsFilter.order);
    }

    /*
     * Call the function to sort path, sending the rights arguments
     */

    public sortPath(firstArg, secondArg, newOrder) {
        let newFilter = {newSort: firstArg, otherSort: secondArg, order: newOrder};
        this.project.showed_dirs =
            sortMapInArray(newFilter, this.pathsFilter, this.project.source_dirs);
    }

    /*
     * Call the function to sort, sending the rights arguments
     */

    public sortFiles(firstArg, secondArg, newOrder) {
        let newFilter = {newSort: firstArg, otherSort: secondArg, order: newOrder};
        this.currentSourceDir.showed_sources =
            sortMapInArray(newFilter, this.filesFilter, this.currentSourceDir.sources);
    }
}
