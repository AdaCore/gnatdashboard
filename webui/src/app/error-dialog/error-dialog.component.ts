import { Component } from '@angular/core';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogRef} from '@angular/material';
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'error-dialog',
    templateUrl: './error-dialog.component.html',
    styleUrls: [ 'error-dialog.component.scss' ]
})

export class ErrorDialogComponent {

    public error: any;

    constructor(public dialogRef: MatDialogRef<ErrorDialogComponent>,
                private reportService: SharedReport) {}

    private ngOnInit(): void {
        this.error = this.reportService.errorToShow;
    }

    public close(): void {
        this.dialogRef.close();
    }

}
