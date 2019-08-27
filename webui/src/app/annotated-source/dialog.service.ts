import { Observable } from 'rxjs';
import { ReviewDialog } from './review-dialog.component';
import { ReviewHistoryDialog } from './review-history-dialog.component';

import { MatDialogRef, MatDialog, MatDialogConfig } from '@angular/material';
import { Injectable } from '@angular/core';

@Injectable()
export class DialogsService {

    constructor(private dialog: MatDialog) {}

    public review(): Observable<boolean> {
        let dialogRef: MatDialogRef<ReviewDialog>;

        dialogRef = this.dialog.open(ReviewDialog);

        return dialogRef.afterClosed();
    }

    public reviewHistory(): Observable<boolean> {
        let dialogRef: MatDialogRef<ReviewHistoryDialog>;

        dialogRef = this.dialog.open(ReviewHistoryDialog);

        return dialogRef.afterClosed();
    }

}
