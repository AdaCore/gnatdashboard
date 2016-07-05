import { RouterConfig } from '@angular/router';

import { About } from './about/about.component';
import { AnnotatedSource } from './annotated-source/annotated-source.component';
import { GNAThubBlob } from './gnathub-blob/gnathub-blob.component';
import { GNAThubReport } from './gnathub-report/gnathub-report.component';
import { TraceList } from './trace-list/trace-list.component';

export const routes: RouterConfig = [
    { path: '',        component: GNAThubReport },
    { path: 'report',  component: GNAThubReport },
    { path: 'blob',    component: GNAThubBlob },
    { path: 'traces',  component: TraceList },
    { path: 'source',  component: AnnotatedSource },
    { path: 'about',   component: About }
];
