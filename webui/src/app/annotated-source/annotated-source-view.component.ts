import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { GNAThubService } from '../gnathub.service';
import { IAnnotatedSourceFile } from 'gnat';

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
        private route: ActivatedRoute) {}

    /** @override */
    public ngOnInit(): void {
        this.filename = this.route.snapshot.params['filename'];
        this.gnathub.getSource(this.filename).subscribe(
            blob => this.blob = blob,
            error => this.isBlobFetchError = !!error);
    }
}
