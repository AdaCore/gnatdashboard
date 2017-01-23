import { NgModule, ApplicationRef } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { MaterialModule } from '@angular/material';
import { RouterModule, PreloadAllModules } from '@angular/router';
import {
    removeNgStyles, createNewHosts, createInputTransfer
} from '@angularclass/hmr';

/*
 * Platform and Environment providers/directives/pipes
 */
import { ENV_PROVIDERS } from './environment';
import { ROUTES } from './main-responder.routes';
import { MainResponderComponent } from './main-responder.component';
import { APP_RESOLVER_PROVIDERS } from './main-responder.resolver';
import { AppState, InteralStateType } from './main-responder.service';

import { AboutComponent } from './about';
import { ArrayNaturalSortPipe } from './array.pipe';
import { AnnotatedSourceComponent } from './annotated-source';
import { CountPipe } from './count.pipe';
import { InlineCommentComponent } from './inline-comment';
import { SourceTreeViewComponent } from './source-tree-view';
import { SpinnerComponent } from './spinner';
import { MapKeysPipe } from './map-keys.pipe';
import { MapValuesPipe } from './map-values.pipe';
import { NotEmptyPipe } from './not-empty.pipe';
import {
    MissingSourceErrorComponent, MissingReportErrorComponent
} from './errors';
import { NoContentComponent } from './no-content';
import { OptionSelectorComponent } from './option-selector';
import { ProjectComponent } from './project';
import { ReportComponent } from './report';
import { SourceListComponent } from './source-list';

import { GNAThubService } from './gnathub.service';
import { Ng2PageScrollModule } from 'ng2-page-scroll';

// Application wide providers
const APP_PROVIDERS = [
    ...APP_RESOLVER_PROVIDERS,
    GNAThubService,
    AppState
];

type StoreType = {
    state: InteralStateType,
    restoreInputValues: () => void,
    disposeOldHosts: () => void
};

/**
 * `AppModule` is the main entry point into Angular2's bootstrap process.
 */
@NgModule({
    bootstrap: [ MainResponderComponent ],
    declarations: [
        MainResponderComponent,
        AboutComponent,
        ArrayNaturalSortPipe,
        AnnotatedSourceComponent,
        CountPipe,
        InlineCommentComponent,
        SpinnerComponent,
        MapKeysPipe,
        MapValuesPipe,
        MissingReportErrorComponent,
        MissingSourceErrorComponent,
        NoContentComponent,
        NotEmptyPipe,
        OptionSelectorComponent,
        ProjectComponent,
        ReportComponent,
        SourceListComponent,
        SourceTreeViewComponent
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        MaterialModule.forRoot(),
        Ng2PageScrollModule.forRoot(),
        RouterModule.forRoot(ROUTES, {
            useHash: true,
            preloadingStrategy: PreloadAllModules
        })
    ],
    providers: [
        ENV_PROVIDERS,
        APP_PROVIDERS
    ]
})
export class AppModule {
    constructor(public appRef: ApplicationRef, public appState: AppState) {
    }

    public hmrOnInit(store: StoreType) {
        if (!store || !store.state) {
            return;
        }
        console.log('HMR store', JSON.stringify(store, null, 2));
        this.appState._state = store.state;
        if ('restoreInputValues' in store) {
            let restoreInputValues = store.restoreInputValues;
            setTimeout(restoreInputValues);
        }
        this.appRef.tick();
        delete store.state;
        delete store.restoreInputValues;
    }

    public hmrOnDestroy(store: StoreType) {
        const cmpLocation = this.appRef.components.map(
            cmp => cmp.location.nativeElement);
        // recreate elements
        const state = this.appState._state;
        store.state = state;
        store.disposeOldHosts = createNewHosts(cmpLocation);
        store.restoreInputValues = createInputTransfer();
        // remove styles
        removeNgStyles();
    }

    public hmrAfterDestroy(store: StoreType) {
        // display new elements
        store.disposeOldHosts();
        delete store.disposeOldHosts;
    }
}
