import { Component, Input } from '@angular/core';
import { IModule, ISourceDir } from 'gnat';

@Component({
    selector: 'project-source-list',
    templateUrl: 'project-source-list.component.html',
    styleUrls: [ 'project-source-list.component.scss' ],
})
export class ProjectSourceListComponent {
    @Input() public project: IModule;
    @Input() public directory: string;

    public getSelectedSourceDir(): ISourceDir {
        if (!this.directory) {
            const sourceDir = this.getFirstSourceDir();
            if (sourceDir) {
                return sourceDir;
            }
            return null;
        }
        return this.project.source_dirs[this.directory];
    }

    public isSelected(sourceDir: string): boolean {
        const selected = this.getSelectedSourceDir();
        if (!selected) {
            return false;
        }
        return selected.path === sourceDir;
    }

    private getFirstSourceDir(): ISourceDir {
        const sourceDirs = Object.keys(this.project.source_dirs);
        if (sourceDirs.length) {
            return this.project.source_dirs[sourceDirs[0]];
        }
        return null;
    }
}
