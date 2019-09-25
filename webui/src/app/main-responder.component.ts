import { Component, ViewEncapsulation } from '@angular/core';
import { SharedReport } from './main-responder.service';
import { CodepeerDialogsService } from './codepeer-run-dialog/codepeer-dialog.service'

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
    providers: [CodepeerDialogsService]
})
export class MainResponderComponent {
    constructor(public reportService: SharedReport,
                private dialog: CodepeerDialogsService) {}

    public showCodepeerRunInfo() {
        this.dialog.codepeerRunInfo().subscribe((data:any) => {});
    }
}
