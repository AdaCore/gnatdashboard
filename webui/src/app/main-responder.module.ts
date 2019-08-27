import { NgModule, ApplicationRef } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { MatDialogModule, MatTooltipModule, MatIconModule } from '@angular/material';
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
import { AppState, InteralStateType, SharedReport } from './main-responder.service';

import { PreferencesComponent } from './preferences';
import {
    AnnotatedSourceComponent,
    AnnotatedSourceContentComponent,
    AnnotatedSourceLineComponent,
    AnnotatedSourceViewComponent
} from './annotated-source';
import { ReviewDialog } from './annotated-source/review-dialog.component'
import { ReviewHistoryDialog } from './annotated-source/review-history-dialog.component'
import { CodepeerRunInfoDialog } from './codepeer-run-dialog/codepeer-run-info-dialog.component'
import { DialogsService } from './annotated-source/dialog.service'
import { CodepeerDialogsService } from './codepeer-run-dialog/codepeer-dialog.service'
import { CountPipe } from './count.pipe';
import { FilterSelectorComponent } from './filter-selector';
import { FilterPanelComponent } from './filter-selector';
import { InlineMessagesComponent } from './inline-messages';
import { InlineAnnotationsComponent } from './inline-annotations';
import { MapKeysPipe } from './map-keys.pipe';
import { MapValuesPipe } from './map-values.pipe';
import {
    MissingSourceErrorComponent, MissingReportErrorComponent
} from './errors';
import { NoContentComponent } from './no-content';
import { NotEmptyPipe } from './not-empty.pipe';
import { CodeExplorerComponent } from './code-explorer';
import { MessageExplorerComponent } from './message-explorer';
import { SpinnerComponent } from './spinner';

import { GNAThubService } from './gnathub.service';
import { NgxPageScrollCoreModule } from 'ngx-page-scroll-core';

// Application wide providers
const APP_PROVIDERS = [
    ...APP_RESOLVER_PROVIDERS,
    GNAThubService,
    AppState,
    SharedReport
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
        PreferencesComponent,
        AnnotatedSourceComponent,
        AnnotatedSourceContentComponent,
        AnnotatedSourceLineComponent,
        AnnotatedSourceViewComponent,
        CountPipe,
        FilterSelectorComponent,
        FilterPanelComponent,
        InlineMessagesComponent,
        InlineAnnotationsComponent,
        MainResponderComponent,
        MapKeysPipe,
        MapValuesPipe,
        MissingReportErrorComponent,
        MissingSourceErrorComponent,
        NoContentComponent,
        NotEmptyPipe,
        CodeExplorerComponent,
        MessageExplorerComponent,
        SpinnerComponent,
        ReviewDialog,
        ReviewHistoryDialog,
        CodepeerRunInfoDialog
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        MatDialogModule,
        MatTooltipModule,
        MatIconModule,
        NgxPageScrollCoreModule,
        RouterModule.forRoot(ROUTES, {
            useHash: true,
            preloadingStrategy: PreloadAllModules
        })
    ],
    providers: [
        APP_PROVIDERS,
        ENV_PROVIDERS
    ],
    entryComponents: [ReviewDialog, ReviewHistoryDialog, CodepeerRunInfoDialog]
})
export class AppModule {
    constructor(public appRef: ApplicationRef, public appState: AppState) {}

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
