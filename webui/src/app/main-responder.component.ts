import { Component, ViewEncapsulation } from '@angular/core';
import { SharedReport } from './main-responder.service';
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
    providers: [ErrorDialogsService]
})

export class MainResponderComponent {
    constructor(public reportService: SharedReport,
                 private errorDialog: ErrorDialogsService) {}

    public showError() {
        this.errorDialog.showError().subscribe((data:any) => {});
    }
}
