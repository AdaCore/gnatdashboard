import { Component } from '@angular/core';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef} from '@angular/material';
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'review-history-dialog',
    templateUrl: './review-history-dialog.component.html',
    styleUrls: [ 'review-history-dialog.component.scss' ]
})

export class ReviewHistoryDialogComponent {

    public history: any;
    public message: any;

    constructor(public dialogRef: MatDialogRef<ReviewHistoryDialogComponent>,
                private reportService: SharedReport) {}

    public ngOnInit(): void {
        this.history = this.reportService.history.reviews;
        this.message = this.reportService.history.message;
    }

    public close(): void {
        this.dialogRef.close();
    }

    public trackReview(index: number, review: any): void {
        return review ? review.date : undefined;
    }
}
