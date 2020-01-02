import { Component, Input } from '@angular/core';

@Component({
    selector: 'missing-source-error',
    templateUrl: './missing-source-error.component.html',
    styleUrls: [ 'errors.scss' ]
})
export class MissingSourceErrorComponent {
    @Input() public filename: string;
}
