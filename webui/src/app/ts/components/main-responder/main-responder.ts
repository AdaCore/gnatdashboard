import { Component, View } from "angular2/angular2";
import { RouteConfig, RouterLink, RouterOutlet } from "angular2/router";

import { About } from "../about/about";
import { Home } from "../home/home";

@Component({ selector: "main-responder" })
@View({
    templateUrl: "app/components/main-responder/main-responder.html",
    directives: [ RouterLink, RouterOutlet ]
})
@RouteConfig([
    { path: "/", as: "Home", component: Home },
    { path: "/about", as: "About", component: About }
])
export class MainResponder { }
