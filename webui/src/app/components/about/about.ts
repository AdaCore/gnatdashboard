import { Component, View } from "angular2/core";

@Component({ selector: "about" })
@View({ templateUrl: "app/components/about/about.html" })
export class About {
    private today: Date;
    constructor () {
        this.today = new Date();
    }
}
