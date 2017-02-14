import { Routes } from '@angular/router';

import { AboutComponent } from './about';
import { AnnotatedSourceViewComponent } from './annotated-source';
import { NoContentComponent } from './no-content';
import { ProjectExplorerComponent } from './project-explorer';

export const ROUTES: Routes = [
    { path: '', redirectTo: '/project-explorer', pathMatch: 'full' },
    { path: 'project-explorer', component: ProjectExplorerComponent },
    { path: 'source/:filename', component: AnnotatedSourceViewComponent },
    { path: 'about',            component: AboutComponent },
    { path: '**',               component: NoContentComponent }
];
