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
    public reviewStatus: any = [
        {
            show: 'Pending',
            value: 'PENDING',
            category: 'PENDING'
        },
        {
            show: 'Not a bug',
            value: 'NOT_A_BUG',
            category: 'NOT_A_BUG'
        },
        {
            show: 'False positive',
            value: 'FALSE_POSITIVE',
            category: 'NOT_A_BUG'
        },
        {
            show: 'Intentional',
            value: 'INTENTIONAL',
            category: 'NOT_A_BUG'
        },
        {
            show: 'Bug',
            value: 'BUG',
            category: 'BUG'
        }];

    constructor(public dialogRef: MatDialogRef<ReviewDialogComponent>,
                public reportService: SharedReport) {}

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
                category = status.category;
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
