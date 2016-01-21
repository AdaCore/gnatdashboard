import { Component } from "angular2/core";
import { RouteConfig, RouterLink, RouterOutlet } from "angular2/router";

import { About } from "../about/about";
import { Summary } from "../summary/summary";

@Component({
    selector: "main-responder",
    templateUrl: "app/components/main-responder/main-responder.html",
    directives: [ RouterLink, RouterOutlet ]
})
@RouteConfig([
    { path: "/", as: "Summary", component: Summary },
    { path: "/traces", as: "Traces", component: Summary },
    { path: "/traces/:program*", as: "ProgramTraces", component: Summary },
    { path: "/about", as: "About", component: About }
])
export class MainResponder { }
