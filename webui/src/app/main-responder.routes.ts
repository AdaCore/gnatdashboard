import { RouterConfig } from '@angular/router';

import { About } from './about/about.component';
import { GNAThubBlob } from './gnathub-blob/gnathub-blob.component';
import { GNAThubReport } from './gnathub-report/gnathub-report.component';

export const routes: RouterConfig = [
    { path: '',        component: GNAThubReport },
    { path: 'report',  component: GNAThubReport },
    { path: 'blob',    component: GNAThubBlob },
    { path: 'about',   component: About }
];
