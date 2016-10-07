import { Component, Input } from '@angular/core';

@Component({
    selector: 'missing-source-error',
    templateUrl: './missing-source-error.component.html',
    styleUrls: [ './errors.css' ]
})
export class MissingSourceError {
    @Input() filename = null;
}
