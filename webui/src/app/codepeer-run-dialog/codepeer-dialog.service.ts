import { Observable } from 'rxjs/Rx';
import { CodepeerRunInfoDialog } from './codepeer-run-info-dialog.component';

import { MdDialogRef, MdDialog, MdDialogConfig } from '@angular/material';
import { Injectable } from '@angular/core';

@Injectable()
export class CodepeerDialogsService {

    constructor(private dialog: MdDialog) {}

    public codepeerRunInfo(): Observable<boolean> {
        let dialogRef: MdDialogRef<CodepeerRunInfoDialog>;

        dialogRef = this.dialog.open(CodepeerRunInfoDialog);

        return dialogRef.afterClosed();
    }

}
