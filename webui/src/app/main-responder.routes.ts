import { Routes } from '@angular/router';

import { PreferencesComponent } from './preferences';
import { AnnotatedSourceViewComponent } from './annotated-source';
import { NoContentComponent } from './no-content';
import { CodeExplorerComponent } from './code-explorer';
import { MessageExplorerComponent } from './message-explorer';
import { CodepeerHistoryComponent } from './codepeer-history';


export const ROUTES: Routes = [
    { path: '', redirectTo: '/message-explorer', pathMatch: 'full' },
    { path: 'code-explorer', component: CodeExplorerComponent },
    { path: 'code-explorer/:project', component: CodeExplorerComponent },
    { path: 'code-explorer/:project/:directory',
     component: CodeExplorerComponent },
    { path: 'message-explorer',   component: MessageExplorerComponent },
    { path: 'source/:filename', component: AnnotatedSourceViewComponent },
    { path: 'codepeer-history', component: CodepeerHistoryComponent},
    { path: 'preferences',      component: PreferencesComponent },
    { path: '**',               component: NoContentComponent }
];
