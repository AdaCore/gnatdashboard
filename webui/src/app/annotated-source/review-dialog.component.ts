import { Component } from '@angular/core';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef } from '@angular/material';
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'review-dialog',
    templateUrl: './review-dialog.component.html',
    styleUrls: [ 'review-dialog.component.scss' ]
})

export class ReviewDialog {

    public username: string;
    public review: string;
    public status: string;
    public category: string;
    public review_status = [
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

    constructor(public dialogRef: MatDialogRef<ReviewDialog>,
                 public reportService: SharedReport) {}

    public cancel() {
        this.dialogRef.close();
    }

    public submit() {
        if (!this.status) {
            this.showError();
            return;
        }

        this.category = this.getCategory(this.status);
        let data = {
            username : this.username,
            review : this.review,
            status  :this.status,
            category  :this.category
        };
        this.dialogRef.close(data);
    }

    private getCategory(my_status): string {
        let category: string = ''
        this.review_status.forEach(function(status){
             if (status.value == my_status){
                category = status.category;
            }
        })
        return category;
    }

    public showError() {
        let elem = document.getElementById('dialogError');
        elem.classList.add('show');
    }

    public trackMsg(index, msg) {
        return msg ? msg.id : undefined;
    }
}
