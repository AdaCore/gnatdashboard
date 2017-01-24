import { Component } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

import { GNAThubService } from '../gnathub.service';
import { IGNAThubBlob } from 'gnat';

@Component({
    selector: 'annotated-source-view',
    templateUrl: './annotated-source-view.component.html'
})
export class AnnotatedSourceViewComponent {
    public filename: string = null;
    public blob: IGNAThubBlob = null;
    public isBlobFetchError: boolean = false;

    constructor(
        private gnathub: GNAThubService,
        private route: ActivatedRoute) {}

    public ngOnInit(): void {
        this.filename = this.route.snapshot.params['filename'];
        this.gnathub.getSource(this.filename).subscribe(
            blob => this.blob = blob,
            error => this.isBlobFetchError = !!error);
    }
}
