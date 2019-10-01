import { Component, ViewEncapsulation } from '@angular/core';
import { SharedReport } from './main-responder.service';
import { CodepeerDialogsService } from './codepeer-run-dialog/codepeer-dialog.service'
import { ErrorDialogsService } from './error-dialog/error-dialog.service'

@Component({
    selector: 'main-responder',
    encapsulation: ViewEncapsulation.None,
    templateUrl: './main-responder.component.html',
    styleUrls: [
        'app.scss',
        'main-responder.component.scss',
        'material-icons.scss',
        'pygments-github.scss'
    ],
    providers: [CodepeerDialogsService, ErrorDialogsService]
})

export class MainResponderComponent {
    constructor(public reportService: SharedReport,
                 private dialog: CodepeerDialogsService,
                 private errorDialog: ErrorDialogsService) {}

    public showCodepeerRunInfo() {
        this.dialog.codepeerRunInfo().subscribe((data:any) => {});
    }
    public showError() {
        this.errorDialog.showError().subscribe((data:any) => {});
    }
}
