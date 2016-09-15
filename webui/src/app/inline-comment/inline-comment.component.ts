import { Component, Input } from '@angular/core';
import { IGNAThubMessage } from 'gnat';

@Component({
    selector: 'inline-comment',
    templateUrl: './inline-comment.template.html',
    styleUrls: [ './inline-comment.style.css' ]
})
export class InlineComment {
    // TODO(delay): @Input() message: IGNAThubMessage = null;
    @Input() message: any = null;
}

