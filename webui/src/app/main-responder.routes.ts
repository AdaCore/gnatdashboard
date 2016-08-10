import { RouterConfig } from '@angular/router';

import { About } from './about';
import { AnnotatedSource } from './annotated-source';
import { GNAThubReport } from './gnathub-report';
import { SourceList } from './source-list';

export const routes: RouterConfig = [
    { path: '',                 component: GNAThubReport },
    { path: 'report',           component: GNAThubReport },
    { path: 'sources',          component: SourceList },
    { path: 'source/:filename', component: AnnotatedSource },
    { path: 'about',            component: About }
];
