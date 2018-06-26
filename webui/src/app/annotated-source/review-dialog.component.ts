import { Component } from '@angular/core';

import { MdDialogRef } from '@angular/material'

@Component({
    selector: 'review-dialog',
    templateUrl: './review-dialog.component.html',
    styleUrls: [ 'review-dialog.component.scss' ]
})

export class ReviewDialog {

    public username: string;
    public review: string;
    public status: string;
    public review_status = ['Uncategorized', 'Pending', 'Not_A_Bug', 'False_Positive', 'Intentional', 'Bug'];

    constructor(public dialogRef: MdDialogRef<ReviewDialog>) {}

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
}
