import {
    ChangeDetectionStrategy,
    Component,
    HostBinding,
    Input,
    OnInit
} from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';

import { IAnnotatedSourceLine, ICoverage } from 'gnat';

@Component({
    selector: 'annotated-source-line',
    templateUrl: './annotated-source-line.component.html',
    styleUrls: [ 'annotated-source-line.component.scss' ],
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class AnnotatedSourceLineComponent implements OnInit {
    @Input() public line: IAnnotatedSourceLine;
    @Input() public coverage: ICoverage;
    public safeHtml: SafeHtml;

    @HostBinding('class.no_code') private isNoCode: boolean = false;
    @HostBinding('class.covered') private isCovered: boolean = false;
    @HostBinding('class.not_covered') private isNotCovered: boolean = false;

    constructor(private sanitizer: DomSanitizer) {}

    /** @override */
    public ngOnInit() {
        this.safeHtml =
            this.sanitizer.bypassSecurityTrustHtml(this.line.html_content);
        if (this.coverage) {
            const status = this.coverage.status.toLowerCase();
            this.isNoCode = status === 'no_code';
            this.isCovered = status === 'covered';
            this.isNotCovered = status === 'not_covered';
        }
    }
}
