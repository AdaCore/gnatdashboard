import { Component, ViewEncapsulation } from '@angular/core';
import { ROUTER_DIRECTIVES } from '@angular/router';

@Component({
    selector: 'main-responder',
    encapsulation: ViewEncapsulation.None,
    templateUrl: './main-responder.template.html',
    styleUrls: [ './app.style.css', './main-responder.style.css' ],
    directives: [ ROUTER_DIRECTIVES ]
})
export class MainResponder {}
