import { Routes } from '@angular/router';

import { AboutComponent } from './about';
import { AnnotatedSourceComponent } from './annotated-source';
import { NoContentComponent } from './no-content';
import { ProjectComponent } from './project';
import { ReportComponent } from './report';
import { SourceListComponent } from './source-list';

export const ROUTES: Routes = [
    { path: '',                 redirectTo: '/report', pathMatch: 'full' },
    { path: 'report',           component: ReportComponent },
    { path: 'project/:name',    component: ProjectComponent },
    { path: 'sources',          component: SourceListComponent },
    { path: 'source/:filename', component: AnnotatedSourceComponent },
    { path: 'about',            component: AboutComponent },
    { path: '**',               component: NoContentComponent }
];
