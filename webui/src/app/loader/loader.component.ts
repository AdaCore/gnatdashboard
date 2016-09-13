import { Component, Input } from '@angular/core';

@Component({
    selector: 'loader',
    templateUrl: './loader.template.html',
    styleUrls: [ './loader.style.css' ]
})
export class Loader {
    @Input() caption: string = null;
}
