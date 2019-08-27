import { Component } from '@angular/core';

import { MatDialogRef} from '@angular/material'
import { SharedReport } from '../main-responder.service';

@Component({
    selector: 'codepeer-run-info-dialog',
    templateUrl: './codepeer-run-info-dialog.component.html',
    styleUrls: [ 'codepeer-run-info-dialog.component.scss' ]
})

export class CodepeerRunInfoDialog {

    public info: any;

    constructor(public dialogRef: MatDialogRef<CodepeerRunInfoDialog>,
                 private reportService: SharedReport) {}

    ngOnInit() {
        this.info = this.reportService.codepeerRunInfo;
    }

    public close() {
        this.dialogRef.close();
    }

}
