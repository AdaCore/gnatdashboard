import { Component } from '@angular/core';

import { MdDialogRef } from '@angular/material';
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
    public review_status = [
        {
            show: 'Pending',
            value: 'PENDING'
        },
        {
            show: 'Not a bug',
            value: 'NOT_A_BUG'
        },
        {
            show: 'False positive',
            value: 'FALSE_POSITIVE'
        },
        {
            show: 'Intentional',
            value: 'INTENTIONAL'
        },
        {
            show: 'Bug',
            value: 'BUG'
        }];

    constructor(public dialogRef: MdDialogRef<ReviewDialog>,
                 public reportService: SharedReport) {}

    public cancel() {
        this.dialogRef.close();
    }

    public submit() {
        if (!this.status) {
            this.showError();
            return;
        }

        let data = {
            username : this.username,
            review : this.review,
            status  :this.status
        };
        this.dialogRef.close(data);
    }

    public showError() {
        let elem = document.getElementById('dialogError');
        elem.classList.add('show');
    }

    public trackMsg(index, msg) {
        return msg ? msg.id : undefined;
    }
}
