import { bootstrap } from "angular2/platform/browser";
import { provide } from "angular2/core";
import { HTTP_PROVIDERS } from "angular2/http";
import {
    HashLocationStrategy, LocationStrategy, ROUTER_PROVIDERS
} from "angular2/router";

import { MainResponder } from "./components/main-responder/main-responder";

// Initialize the AngularJS application
bootstrap(MainResponder, [
    HTTP_PROVIDERS,
    ROUTER_PROVIDERS,
    provide(LocationStrategy, { useClass: HashLocationStrategy })
]);
