import { Observable } from 'rxjs';
import { ErrorDialogComponent } from './error-dialog.component';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef, MatDialog, MatDialogConfig } from '@angular/material/dialog';
import { Injectable } from '@angular/core';

@Injectable()
export class ErrorDialogsService {

    constructor(private dialog: MatDialog) {}

    public showError(): Observable<boolean> {
        let dialogRef: MatDialogRef<ErrorDialogComponent>;

        dialogRef = this.dialog.open(ErrorDialogComponent);

        return dialogRef.afterClosed();
    }

}
