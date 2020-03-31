import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { SharedReport } from '../main-responder.service';
import { GNAThubService } from '../gnathub.service';
import { IAnnotatedSourceFile } from 'gnat';
import { Http, Response } from '@angular/http';

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
        private http: Http) {}

    /** @override */
    public ngOnInit(): void {
        this.filename = this.route.snapshot.params['filename'];
        const url: string = this.reportService.url + 'source/' +  this.filename + '.json';
        this.http.get(url).subscribe(
            data => {
                this.blob = JSON.parse(data['_body']);
            }, error => {
                console.error('[Error] LoadFile: ', error);
                this.isBlobFetchError = true;
            }
        );
    }
}
