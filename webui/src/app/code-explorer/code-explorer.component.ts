import { Component, OnDestroy, OnInit, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { SharedReport } from '../main-responder.service';
import { sortCodeArray } from '../utils/sortArray';
import { storeProjectSort } from '../utils/dataStorage';
import { ISort, IModule, ISourceNav, ISourceDir, ISource } from 'gnat';

@Component({
    selector: 'code-explorer',
    templateUrl: './code-explorer.component.html',
    styleUrls: [ 'code-explorer.component.scss' ]
})
export class CodeExplorerComponent implements OnInit, OnDestroy {
    public project: string;
    public directory: string;
    private sub: Subscription;

    constructor(private route: ActivatedRoute,
                public reportService: SharedReport,
                @Inject(DOCUMENT) private document: Document) {}
    /** @override */
    public ngOnInit(): void {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
        });
        this.reportService.page = 'code-explorer';
        localStorage.setItem('defaultMain', '/code-explorer');
    }

    /** @override */
    public ngOnDestroy(): void {
        this.sub.unsubscribe();
    }

    public sortModules(firstSort: string, secondSort: string): void {
        let newFilter: ISort = {newSort: firstSort, otherSort: secondSort};
        this.reportService.code.modules =
            sortCodeArray(newFilter,
                          this.reportService.projectSort,
                          this.reportService.code.modules);
        storeProjectSort(newFilter);
    }

    public expandCollapseAll(myValue: boolean): void {
        if (this.reportService.checkArray(this.reportService.code.modules,
                                          'code-explorer.component',
                                          'expandCollapseAll', 'reportService.code.modules')) {
            this.reportService.code.modules.forEach(function(project: IModule): void {
                project.expand = myValue;

                if (this.reportService.checkArray(project.source_dirs,
                                                  'code-explorer.component',
                                                  'expandCollapseAll', 'project.source_dirs')) {
                    project.source_dirs.forEach(function(source: ISourceDir): void {
                        source.expand = myValue;
                    }.bind(this));
                }
            }.bind(this));
        }
    }

    public openClose(source: ISourceDir): void {
        source.expand = !source.expand;
    }

    public showFilesChanges(): void {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

    public trackProject(index: number, project: IModule): string {
        return project ? project.name : undefined;
    }

    public trackFolder(index: number, folder: ISourceDir): string {
        return folder ? folder.name : undefined;
    }

    public trackFile(index: number, file: ISource): string {
        return file ? file.filename : undefined;
    }

}
