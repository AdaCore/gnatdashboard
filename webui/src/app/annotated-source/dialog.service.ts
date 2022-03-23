import { Observable } from 'rxjs';
import { ReviewDialogComponent } from './review-dialog.component';
import { ReviewHistoryDialogComponent } from './review-history-dialog.component';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef, MatDialog, MatDialogConfig } from '@angular/material/dialog';
import { Injectable } from '@angular/core';

@Injectable()
export class DialogsService {

    constructor(private dialog: MatDialog) {}

    public reviewHistory(): Observable<boolean> {
        let dialogRef: MatDialogRef<ReviewHistoryDialogComponent>;

        dialogRef = this.dialog.open(ReviewHistoryDialogComponent);

        return dialogRef.afterClosed();
    }

}
