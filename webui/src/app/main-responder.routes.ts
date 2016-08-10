import { RouterConfig } from '@angular/router';

import { About } from './about';
import { AnnotatedSource } from './annotated-source';
import { Report } from './report';
import { SourceList } from './source-list';

export const routes: RouterConfig = [
    { path: '',                 component: Report },
    { path: 'report',           component: Report },
    { path: 'sources',          component: SourceList },
    { path: 'source/:filename', component: AnnotatedSource },
    { path: 'about',            component: About }
];
