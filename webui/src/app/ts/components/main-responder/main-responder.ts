import { Component } from "angular2/core";
import { RouteConfig, RouterLink, RouterOutlet } from "angular2/router";

import { About } from "../about/about";
import { AnnotatedSource } from "../annotated-source/annotated-source";
import { GNAThubReport } from "../gnathub-report/gnathub-report";
import { TraceList } from "../trace-list/trace-list";

@Component({
    selector: "main-responder",
    templateUrl: "app/components/main-responder/main-responder.html",
    directives: [ RouterLink, RouterOutlet ]
})
@RouteConfig([
    { path: "/", as: "Report", component: GNAThubReport },
    { path: "/report", as: "Report", component: GNAThubReport },
    { path: "/traces", as: "Traces", component: TraceList },
    { path: "/source", as: "Source", component: AnnotatedSource },
    { path: "/about", as: "About", component: About }
])
export class MainResponder { }
