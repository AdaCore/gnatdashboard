import { Observable } from 'rxjs/Rx';
import { ReviewDialog } from './review-dialog.component';
import { ReviewHistoryDialog } from './review-history-dialog.component';

import { MdDialogRef, MdDialog, MdDialogConfig } from '@angular/material';
import { Injectable } from '@angular/core';

@Injectable()
export class DialogsService {

    constructor(private dialog: MdDialog) {}

    public review(): Observable<boolean> {
        let dialogRef: MdDialogRef<ReviewDialog>;

        dialogRef = this.dialog.open(ReviewDialog);

        return dialogRef.afterClosed();
    }

    public reviewHistory(): Observable<boolean> {
        let dialogRef: MdDialogRef<ReviewHistoryDialog>;

        dialogRef = this.dialog.open(ReviewHistoryDialog);

        return dialogRef.afterClosed();
    }

}
