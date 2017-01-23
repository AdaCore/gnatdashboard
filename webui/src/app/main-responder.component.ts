import { Component, ViewEncapsulation } from '@angular/core';
import { PageScrollConfig } from 'ng2-page-scroll';

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
    constructor() {
        PageScrollConfig.defaultDuration = 0;
    }
}
