import { Component } from '@angular/core';

@Component({
    selector: 'about',
    templateUrl: './about.template.html'
})
export class About {
    private today: Date;
    constructor () {
        this.today = new Date();
    }
}
