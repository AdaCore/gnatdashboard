import { RouterConfig } from '@angular/router';

import { About } from './about/about.component';
import { AnnotatedSource } from './annotated-source/annotated-source.component';
import { GNAThubReport } from './gnathub-report/gnathub-report.component';
import { SourceList } from './source-list/source-list.component';

export const routes: RouterConfig = [
    { path: '',                 component: GNAThubReport },
    { path: 'report',           component: GNAThubReport },
    { path: 'sources',          component: SourceList },
    { path: 'source/:filename', component: AnnotatedSource },
    { path: 'about',            component: About }
];
