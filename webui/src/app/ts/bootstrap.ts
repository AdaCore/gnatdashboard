import { bootstrap } from "angular2/angular2";
import { ROUTER_PROVIDERS } from "angular2/router";

import { MainResponder } from "./components/main-responder/main-responder";

bootstrap(MainResponder, [ ROUTER_PROVIDERS ]);
