import { IGNAThubBlob } from 'gnat';
import { Component, Input } from '@angular/core';

@Component({
    selector: 'filter-list',
    templateUrl: './filter-list.component.html',
    styleUrls: [ 'filter-list.component.scss' ]
})
export class FilterListComponent {
    @Input() public blob: IGNAThubBlob = null;
}
