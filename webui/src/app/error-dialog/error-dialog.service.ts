import { Observable } from 'rxjs';
import { ErrorDialog } from './error-dialog.component';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef, MatDialog, MatDialogConfig } from '@angular/material';
import { Injectable } from '@angular/core';

@Injectable()
export class ErrorDialogsService {

    constructor(private dialog: MatDialog) {}

    public showError(): Observable<boolean> {
        let dialogRef: MatDialogRef<ErrorDialog>;

        dialogRef = this.dialog.open(ErrorDialog);

        return dialogRef.afterClosed();
    }

}
