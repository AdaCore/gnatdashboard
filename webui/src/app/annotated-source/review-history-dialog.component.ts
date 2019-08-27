import { Component } from '@angular/core';

import { MatDialogRef} from '@angular/material'
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'review-history-dialog',
    templateUrl: './review-history-dialog.component.html',
    styleUrls: [ 'review-history-dialog.component.scss' ]
})

export class ReviewHistoryDialog {

    public history: any;
    public message: any;

    constructor(public dialogRef: MatDialogRef<ReviewHistoryDialog>,
                 private reportService: SharedReport) {}

    ngOnInit() {
        this.history = this.reportService.history.reviews;
        this.message = this.reportService.history.message;
    }

    public close() {
        this.dialogRef.close();
    }

    public trackReview(index, review) {
        return review ? review.date : undefined;
    }
}
