import { Component } from 'angular2/core';
import { CORE_DIRECTIVES } from 'angular2/common';

@Component({
    selector: 'loader',
    template: '<div class="loader">Loading...</div>',
    styles: [ require('./loader.css').toString() ]
})
export class Loader { }
