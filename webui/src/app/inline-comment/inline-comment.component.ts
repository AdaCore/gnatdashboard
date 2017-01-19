import { Component, Input } from '@angular/core';
import { IGNAThubMessage } from 'gnat';

@Component({
    selector: 'inline-comment',
    templateUrl: './inline-comment.component.html',
    styleUrls: [ 'inline-comment.component.scss' ]
})
export class InlineCommentComponent {
    // TODO(delay): @Input() message: IGNAThubMessage = null;
    @Input() public message: any = null;
}
