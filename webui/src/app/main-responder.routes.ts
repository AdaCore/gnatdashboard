import { Routes } from '@angular/router';

import { About } from './about';
import { AnnotatedSource } from './annotated-source';
import { NoContent } from './no-content';
import { Project } from './project';
import { Report } from './report';
import { SourceList } from './source-list';

export const ROUTES: Routes = [
    { path: '',                 component: Report },
    { path: 'project/:name',    component: Project },
    { path: 'sources',          component: SourceList },
    { path: 'source/:filename', component: AnnotatedSource },
    { path: 'about',            component: About },
    { path: '**',               component: NoContent }
];
