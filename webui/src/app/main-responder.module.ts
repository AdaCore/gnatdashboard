import { NgModule, ApplicationRef } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { BrowserAnimationsModule } from  '@angular/platform-browser/animations';
import {MatDialogModule} from '@angular/material/dialog'
import {MatTooltipModule} from "@angular/material/tooltip";
import { MatIconModule } from '@angular/material/icon';
import { RouterModule, PreloadAllModules } from '@angular/router';
/*
 * Platform and Environment providers/directives/pipes
 */
import { ENV_PROVIDERS } from './environment';
import { ROUTES } from './main-responder.routes';
import { MainResponderComponent } from './main-responder.component';
import { AppState, InteralStateType, SharedReport } from './main-responder.service';
import {
  AnnotatedSourceComponent,
  AnnotatedSourceContentComponent,
  AnnotatedSourceLineComponent,
  AnnotatedSourceViewComponent
} from './annotated-source';
import { ReviewHistoryDialogComponent } from './annotated-source/review-history-dialog.component';
import { DialogsService } from './annotated-source/dialog.service';
import { ErrorDialogComponent } from './error-dialog/error-dialog.component';
import { ErrorDialogsService } from './error-dialog/error-dialog.service';
import { FilterSelectorComponent } from './filter-selector';
import { FilterPanelComponent } from './filter-selector';
import { InlineAnnotationsComponent } from './inline-annotations';
import {
  MissingSourceErrorComponent, MissingReportErrorComponent
} from './errors';
import { NoContentComponent } from './no-content';
import { NotEmptyPipe } from './not-empty.pipe';
import { CodeExplorerComponent } from './code-explorer';
import { MessageExplorerComponent } from './message-explorer';
import { SpinnerComponent } from './spinner';

import { GNAThubService } from './gnathub.service';
import {HttpClientModule} from "@angular/common/http";
import {APP_BASE_HREF, CommonModule, HashLocationStrategy, LocationStrategy} from "@angular/common";
import {HIGHLIGHT_OPTIONS, HighlightModule} from "ngx-highlightjs";

// Application wide providers
const APP_PROVIDERS: any[] = [
  GNAThubService,
  AppState,
  SharedReport
];
/**
 * `AppModule` is the main entry point into Angular2's bootstrap process.
 */
@NgModule({
    bootstrap: [MainResponderComponent],
    declarations: [
        AnnotatedSourceComponent,
        AnnotatedSourceContentComponent,
        AnnotatedSourceLineComponent,
        AnnotatedSourceViewComponent,
        FilterSelectorComponent,
        FilterPanelComponent,
        InlineAnnotationsComponent,
        MainResponderComponent,
        MissingReportErrorComponent,
        MissingSourceErrorComponent,
        NoContentComponent,
        NotEmptyPipe,
        CodeExplorerComponent,
        MessageExplorerComponent,
        SpinnerComponent,
        ReviewHistoryDialogComponent,
        ErrorDialogComponent
    ],
    imports: [
        BrowserModule,
        CommonModule,
        FormsModule,
        HttpClientModule,
        BrowserAnimationsModule,
        MatDialogModule,
        MatTooltipModule,
        MatIconModule,
        RouterModule.forRoot(ROUTES, {
            useHash: true,
            preloadingStrategy: PreloadAllModules
        }),
        HighlightModule
    ],
    providers: [
        APP_PROVIDERS,
        { provide: LocationStrategy, useClass: HashLocationStrategy },
        { provide: APP_BASE_HREF, useValue: './' },
        {
            provide: HIGHLIGHT_OPTIONS,
            useValue: {
                coreLibraryLoader: () => import('highlight.js/lib/core'),
                languages: {
                    ada: () => import('highlight.js/lib/languages/ada'),
                },
            },
        },
    ]
})
export class AppModule {
  constructor(public appRef: ApplicationRef, public appState: AppState) {}

}
