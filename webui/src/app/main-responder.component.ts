import { Component, ViewEncapsulation } from '@angular/core';
import { PageScrollConfig } from 'ng2-page-scroll';
import { SharedReport } from './main-responder.service';

@Component({
    selector: 'main-responder',
    encapsulation: ViewEncapsulation.None,
    templateUrl: './main-responder.component.html',
    styleUrls: [
        'app.scss',
        'main-responder.component.scss',
        'material-icons.scss',
        'pygments-github.scss'
    ]
})
export class MainResponderComponent {
    constructor(public reportService: SharedReport) {
        PageScrollConfig.defaultDuration = 0;
    }
}
