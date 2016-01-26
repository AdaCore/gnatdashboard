import { Component } from "angular2/core";
import { RouteConfig, RouterLink, RouterOutlet } from "angular2/router";

import { About } from "../about/about";
import { AnnotatedSource } from "../annotated-source/annotated-source";
import { TraceList } from "../trace-list/trace-list";

@Component({
    selector: "main-responder",
    templateUrl: "app/components/main-responder/main-responder.html",
    directives: [ RouterLink, RouterOutlet ]
})
@RouteConfig([
    { path: "/", as: "TraceList", component: TraceList },
    { path: "/traces", as: "Traces", component: TraceList },
    { path: "/source", as: "Source", component: AnnotatedSource },
    { path: "/about", as: "About", component: About }
])
export class MainResponder { }
