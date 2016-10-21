import { Component, Input } from '@angular/core';
import { IGNAThubMessage } from 'gnat';

@Component({
    selector: 'inline-comment',
    templateUrl: './inline-comment.component.html',
    styleUrls: [ 'inline-comment.component.scss' ]
})
export class InlineComment {
    // TODO(delay): @Input() message: IGNAThubMessage = null;
    @Input() message: any = null;
}

