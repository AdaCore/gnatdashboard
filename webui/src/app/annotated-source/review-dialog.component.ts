import { Component } from '@angular/core';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef } from '@angular/material';
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'review-dialog',
    templateUrl: './review-dialog.component.html',
    styleUrls: [ 'review-dialog.component.scss' ]
})

export class ReviewDialogComponent {

    public username: string;
    public review: string;
    public status: string;
    public category: string;
    public reviewStatus: any = [];

    constructor(public dialogRef: MatDialogRef<ReviewDialogComponent>,
                public reportService: SharedReport) {
        this.reviewStatus = this.reportService.globalReviewStatus;
    }

    public cancel(): void {
        this.dialogRef.close();
    }

    public submit(): void {
        if (!this.status) {
            this.showError();
            return;
        }

        this.category = this.getCategory(this.status);
        let data: any = {
            username: this.username,
            review: this.review,
            status: this.status,
            category: this.category
        };
        this.dialogRef.close(data);
    }

    private getCategory(myStatus: string): string {
        let category: string = '';
        this.reviewStatus.forEach(function(status: any): void {
             if (status.value === myStatus){
                category = status.kind;
            }
        });
        return category;
    }

    public showError(): void {
        let elem: HTMLElement = document.getElementById('dialogError');
        elem.classList.add('show');
    }

    public trackMsg(index: number, msg: any): number {
        return msg ? msg.id : undefined;
    }
}
