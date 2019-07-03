import { Observable } from 'rxjs';
import { CodepeerRunInfoDialog } from './codepeer-run-info-dialog.component';

import { MatDialogRef, MatDialog, MatDialogConfig } from '@angular/material';
import { Injectable } from '@angular/core';

@Injectable()
export class CodepeerDialogsService {

    constructor(private dialog: MatDialog) {}

    public codepeerRunInfo(): Observable<boolean> {
        let dialogRef: MatDialogRef<CodepeerRunInfoDialog>;

        dialogRef = this.dialog.open(CodepeerRunInfoDialog);

        return dialogRef.afterClosed();
    }

}
