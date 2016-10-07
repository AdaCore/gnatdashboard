import { Component, Input } from '@angular/core';

@Component({
    selector: 'loader',
    templateUrl: './loader.component.html',
    styleUrls: [ './loader.component.css' ]
})
export class Loader {
    @Input() caption: string = null;
}
