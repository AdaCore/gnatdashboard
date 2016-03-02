/**
 * Providers provided by Angular
 */
import { ELEMENT_PROBE_PROVIDERS, bootstrap } from "angular2/platform/browser";
import { enableProdMode, provide } from "angular2/core";
import { HTTP_PROVIDERS } from "angular2/http";
import {
    HashLocationStrategy, LocationStrategy, ROUTER_PROVIDERS
} from "angular2/router";

const ENV_PROVIDERS = [];

if ('production' === process.env.ENV) {
    enableProdMode();
} else {
    ENV_PROVIDERS.push(ELEMENT_PROBE_PROVIDERS);
}

/**
 * App Component
 * Our top level component that holds all our components.
 */
import { MainResponder } from "./app/components/main-responder/main-responder";

/**
 * Bootstrap our Angular app with a top level component `MainResponder` and
 * inject our Services and Providers into Angular's dependency injection
 * mechanism.
 */
document.addEventListener('DOMContentLoaded', (): void => {
    bootstrap(MainResponder, [
        ...ENV_PROVIDERS,
        ...HTTP_PROVIDERS,
        ...ROUTER_PROVIDERS,
        provide(LocationStrategy, { useClass: HashLocationStrategy })
    ])
    .catch(err => console.error(err));
});

// Include the CSS (handled by webpack)
require('./main.css');
