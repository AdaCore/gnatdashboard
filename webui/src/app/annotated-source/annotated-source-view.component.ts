import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { SharedReport } from '../main-responder.service';
import { GNAThubService } from '../gnathub.service';
import { IAnnotatedSourceFile } from 'gnat';
import {HttpClient} from "@angular/common/http";

import {LoadJsonService} from "../load-json.service";

@Component({
    selector: 'annotated-source-view',
    templateUrl: 'annotated-source-view.component.html'
})
export class AnnotatedSourceViewComponent implements OnInit {
    public filename: string;
    public blob: IAnnotatedSourceFile;
    public isBlobFetchError: boolean = false;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute,
        public reportService: SharedReport,
        private loadJSONService: LoadJsonService) {}

    /** @override */
    public ngOnInit(): void {
        this.filename = this.route.snapshot.params['filename'];
        const url: string = 'source/' +  this.filename + '.js';
        this.loadJSONService.getJSON(url).subscribe(
          (data : IAnnotatedSourceFile) => {
                this.blob = data;
            }, error => {
                console.error('[Error] LoadFile: ', error);
                this.isBlobFetchError = true;
            }
        );
    }
}
