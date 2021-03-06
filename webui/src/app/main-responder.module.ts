import { NgModule, ApplicationRef } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { HttpModule } from '@angular/http';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import { MatDialogModule, MatTooltipModule, MatIconModule } from '@angular/material';
import { RouterModule, PreloadAllModules } from '@angular/router';
import {
    removeNgStyles, createNewHosts, createInputTransfer
} from '@angularclass/hmr';
import { NgxChartsModule } from '@swimlane/ngx-charts';
/*
 * Platform and Environment providers/directives/pipes
 */
import { ENV_PROVIDERS } from './environment';
import { ROUTES } from './main-responder.routes';
import { MainResponderComponent } from './main-responder.component';
import { APP_RESOLVER_PROVIDERS } from './main-responder.resolver';
import { AppState, InteralStateType, SharedReport } from './main-responder.service';
import { ValuesPipe, KeysPipe} from './pipes';
import { PreferencesComponent } from './preferences';
import {
    AnnotatedSourceComponent,
    AnnotatedSourceContentComponent,
    AnnotatedSourceLineComponent,
    AnnotatedSourceViewComponent
} from './annotated-source';
import { ReviewDialogComponent } from './annotated-source/review-dialog.component';
import { ReviewHistoryDialogComponent } from './annotated-source/review-history-dialog.component';
import { DialogsService } from './annotated-source/dialog.service';
import { ErrorDialogComponent } from './error-dialog/error-dialog.component';
import { ErrorDialogsService } from './error-dialog/error-dialog.service';
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
import { CodepeerHistoryComponent } from './codepeer-history';
import { SpinnerComponent } from './spinner';

import { GNAThubService } from './gnathub.service';
import { ScrollToModule } from '@nicky-lenaers/ngx-scroll-to';

// Application wide providers
const APP_PROVIDERS: any[] = [
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
        CodepeerHistoryComponent,
        SpinnerComponent,
        ReviewDialogComponent,
        ReviewHistoryDialogComponent,
        ErrorDialogComponent,
        KeysPipe,
        ValuesPipe
    ],
    imports: [
        BrowserModule,
        FormsModule,
        HttpModule,
        BrowserAnimationsModule,
        MatDialogModule,
        MatTooltipModule,
        MatIconModule,
        NgxChartsModule,
        ScrollToModule.forRoot(),
        RouterModule.forRoot(ROUTES, {
            useHash: true,
            preloadingStrategy: PreloadAllModules
        })
    ],
    providers: [
        APP_PROVIDERS,
        ENV_PROVIDERS
    ],
    entryComponents: [ReviewDialogComponent, ReviewHistoryDialogComponent, ErrorDialogComponent]
})
export class AppModule {
    constructor(public appRef: ApplicationRef, public appState: AppState) {}

    public hmrOnInit(store: StoreType): void {
        if (!store || !store.state) {
            return;
        }
        console.log('HMR store', JSON.stringify(store, null, 2));
        this.appState._state = store.state;
        if ('restoreInputValues' in store) {
            let restoreInputValues: any = store.restoreInputValues;
            setTimeout(restoreInputValues);
        }
        this.appRef.tick();
        delete store.state;
        delete store.restoreInputValues;
    }

    public hmrOnDestroy(store: StoreType): void {
        const cmpLocation: any = this.appRef.components.map(
            cmp => cmp.location.nativeElement);
        // recreate elements
        const state: InteralStateType = this.appState._state;
        store.state = state;
        store.disposeOldHosts = createNewHosts(cmpLocation);
        store.restoreInputValues = createInputTransfer();
        // remove styles
        removeNgStyles();
    }

    public hmrAfterDestroy(store: StoreType): void {
        // display new elements
        store.disposeOldHosts();
        delete store.disposeOldHosts;
    }
}
