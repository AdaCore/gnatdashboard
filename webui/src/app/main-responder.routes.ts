import { Routes } from '@angular/router';

import { PreferencesComponent } from './preferences';
import { AnnotatedSourceViewComponent } from './annotated-source';
import { NoContentComponent } from './no-content';
import { ProjectExplorerComponent } from './project-explorer';
import { ErrorExplorerComponent } from './error-explorer';

export const ROUTES: Routes = [
    { path: '', redirectTo: '/project-explorer', pathMatch: 'full' },
    { path: 'project-explorer', component: ProjectExplorerComponent },
    { path: 'project-explorer/:project', component: ProjectExplorerComponent },
    { path: 'project-explorer/:project/:directory',
     component: ProjectExplorerComponent },
    { path: 'error-explorer',   component: ErrorExplorerComponent },
    { path: 'source/:filename', component: AnnotatedSourceViewComponent },
    { path: 'preferences',      component: PreferencesComponent },
    { path: '**',               component: NoContentComponent }
];
