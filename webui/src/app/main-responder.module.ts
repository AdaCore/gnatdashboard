import { NgModule, ApplicationRef } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { MaterialModule } from '@angular/material';
import { RouterModule } from '@angular/router';
import { removeNgStyles, createNewHosts } from '@angularclass/hmr';

/*
 * Platform and Environment providers/directives/pipes
 */
import { ENV_PROVIDERS } from './environment';
import { ROUTES } from './main-responder.routes';
import { MainResponder } from './main-responder.component';
import { APP_RESOLVER_PROVIDERS } from './main-responder.resolver';
import { AppState, InteralStateType } from './main-responder.service';

import { About } from './about';
import { ArrayNaturalSortPipe } from './array.pipe';
import { AnnotatedSource } from './annotated-source';
import { AutoScroll } from './scroll.directive';
import { CountPipe } from './count.pipe';
import { InlineComment } from './inline-comment';
import { Spinner } from './spinner';
import { MapKeysPipe, MapValuesPipe, NotEmptyPipe } from './object.pipe';
import { MissingSourceError, MissingReportError } from './errors';
import { NoContent } from './no-content';
import { OptionSelector } from './option-selector';
import { Project } from './project';
import { Report } from './report';
import { SourceList } from './source-list';

// Application wide providers
const APP_PROVIDERS = [
    ...APP_RESOLVER_PROVIDERS,
    AppState
];

type StoreType = {
    state: InteralStateType,
    disposeOldHosts: () => void
};

/**
 * `AppModule` is the main entry point into Angular2's bootstrap process.
 */
@NgModule({
    bootstrap: [MainResponder],
    declarations: [
        MainResponder,
        About,
        ArrayNaturalSortPipe,
        AnnotatedSource,
        AutoScroll,
        CountPipe,
        InlineComment,
        Spinner,
        MapKeysPipe,
        MapValuesPipe,
        MissingReportError,
        MissingSourceError,
        NoContent,
        NotEmptyPipe,
        OptionSelector,
        Project,
        Report,
        SourceList
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        MaterialModule.forRoot(),
        RouterModule.forRoot(ROUTES, {useHash: true})
    ],
    providers: [
        ENV_PROVIDERS,
        APP_PROVIDERS
    ]
})
export class AppModule {
    constructor(public appRef: ApplicationRef, public appState: AppState) {
    }

    hmrOnInit(store: StoreType) {
        if (!store || !store.state) return;
        console.log('HMR store', store);
        this.appState._state = store.state;
        this.appRef.tick();
        delete store.state;
    }

    hmrOnDestroy(store: StoreType) {
        const cmpLocation = this.appRef.components.map(
            cmp => cmp.location.nativeElement);
        // recreate elements
        const state = this.appState._state;
        store.state = state;
        store.disposeOldHosts = createNewHosts(cmpLocation);
        // remove styles
        removeNgStyles();
    }

    hmrAfterDestroy(store: StoreType) {
        // display new elements
        store.disposeOldHosts();
        delete store.disposeOldHosts;
    }
}
