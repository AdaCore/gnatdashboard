import { Component } from '@angular/core';

import { MdDialogRef} from '@angular/material'
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'review-history-dialog',
    templateUrl: './review-history-dialog.component.html',
    styleUrls: [ 'review-history-dialog.component.scss' ]
})

export class ReviewHistoryDialog {

    public history: any;

    constructor(public dialogRef: MdDialogRef<ReviewHistoryDialog>,
                private reportService: SharedReport) {}

    ngOnInit() {
        this.history = this.reportService.history;
    }

    public close() {
        this.dialogRef.close();
    }
}
