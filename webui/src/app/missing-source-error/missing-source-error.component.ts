import { Component, Input } from '@angular/core';

@Component({
    selector: 'missing-source-error',
    templateUrl: './missing-source-error.template.html',
    styleUrls: [ './missing-source-error.style.css' ]
})
export class MissingSourceError {
    @Input() filename = null;
}
