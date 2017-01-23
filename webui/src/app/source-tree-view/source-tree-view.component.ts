import { Component, Input } from '@angular/core';

import { IGNAThubReport } from 'gnat';

@Component({
    selector: 'source-tree-view',
    templateUrl: './source-tree-view.component.html',
    styleUrls: [ 'source-tree-view.component.scss' ],
})
export class SourceTreeViewComponent {
    @Input() public report: IGNAThubReport = null;
}
