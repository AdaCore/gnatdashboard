import { RouterConfig } from '@angular/router';

import { About } from './about/about.component';
import { GNAThubBlob } from './gnathub-blob/gnathub-blob.component';
import { GNAThubReport } from './gnathub-report/gnathub-report.component';
import { SourceList } from './source-list/source-list.component';

export const routes: RouterConfig = [
    { path: '',                 component: GNAThubReport },
    { path: 'report',           component: GNAThubReport },
    { path: 'sources',          component: SourceList },
    { path: 'source/:filename', component: GNAThubBlob },
    { path: 'about',            component: About }
];
