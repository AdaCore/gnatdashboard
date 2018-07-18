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

    public openClose(id: string) {
        let elem = this.document.getElementById(id);
        elem.classList.toggle('reduce');
        elem.classList.toggle('open');
    }

    public expandCollapseAll(badClass: string) {
        this.reportService.code.modules.forEach(function(project){
            let elem = this.document.getElementById(project.name);
            let idx = elem.classList.contains(badClass);
            if (idx) {
                this.openClose(project.name);
            }

            project.source_dirs.forEach(function(source){
                let elem = this.document.getElementById(source.name);
                let idx = elem.classList.contains(badClass);
                if (idx) {
                    this.openClose(source.name);
                }
            }.bind(this));

        }.bind(this));
    }

    public showFilesChanges() {
        this.reportService.showFiles = !this.reportService.showFiles;
    }

}
