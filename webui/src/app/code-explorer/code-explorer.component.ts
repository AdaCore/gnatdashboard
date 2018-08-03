import { Component, OnDestroy, OnInit, Inject } from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { SharedReport } from '../main-responder.service';
import { sortCodeArray } from '../utils/sortArray';

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
    public ngOnInit() {
        this.sub = this.route.params.subscribe(params => {
            this.directory = params['directory'];
            this.project = params['project'];
        });
        this.reportService.page = 'code-explorer';
    }

    /** @override */
    public ngOnDestroy() {
        this.sub.unsubscribe();
    }

    public sortModules(firstSort: string, secondSort: string) {
        this.reportService.code.modules =
            sortCodeArray({newSort: firstSort, otherSort: secondSort},
                          this.reportService.codeFilter,
                          this.reportService.code.modules);
    }

    public expandCollapseAll(myValue:boolean) {
        if (this.reportService.checkArray(this.reportService.code.modules,
                                          "code-explorer.component",
                                          "expandCollapseAll", "reportService.code.modules")) {
            this.reportService.code.modules.forEach(function(project){
                project.expand = myValue;

                if (this.reportService.checkArray(project.source_dirs,
                                                  "code-explorer.component",
                                                  "expandCollapseAll", "project.source_dirs")) {
                    project.source_dirs.forEach(function(source){
                        source.expand = myValue;
                    }.bind(this));
                }
            }.bind(this));
        }
    }

    public openClose(source) {
        source.expand = !source.expand;
    }

    public showFilesChanges() {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

}
