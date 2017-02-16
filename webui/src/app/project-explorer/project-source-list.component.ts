import {
    Component,
    Input,
    OnChanges
} from '@angular/core';
import { IModule, ISourceDir } from 'gnat';
import * as naturalSort from 'natural-sort';

@Component({
    selector: 'project-source-list',
    templateUrl: 'project-source-list.component.html',
    styleUrls: [ 'project-source-list.component.scss' ],
})
export class ProjectSourceListComponent implements OnChanges {
    @Input() public project: IModule;
    @Input() public directory: string;

    public currentSourceDir: ISourceDir;

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
    }
}
