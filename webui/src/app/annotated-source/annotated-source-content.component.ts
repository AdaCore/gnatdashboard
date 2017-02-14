import {
    Component,
    Input,
    OnDestroy,
    OnInit
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import {
    IAnnotatedSourceLine,
    IAnnotatedSourceMessage,
    ICoverage,
    ITool
} from 'gnat';

type MessagesByToolId = { [toolId: number]: IAnnotatedSourceMessage[] };

@Component({
    selector: 'annotated-source-content',
    templateUrl: './annotated-source-content.component.html',
    styleUrls: [ 'annotated-source-content.component.scss' ]
})
export class AnnotatedSourceContentComponent implements OnDestroy, OnInit {
    @Input() public lines: IAnnotatedSourceLine[];
    @Input() public tools: { [toolId: number]: ITool };
    @Input() public coverage: { [line: number]: ICoverage };
    @Input() public inlineMessages: { [line: number]: MessagesByToolId };
    @Input() public displayMessages;

    public selectedLine: number;
    private paramSubscription: Subscription;

    constructor(private route: ActivatedRoute) {}

    public ngOnInit() {
        this.paramSubscription = this.route.params.subscribe(params => {
            this.selectedLine = +params['line'];
        });
    }

    public ngOnDestroy() {
        if (this.paramSubscription) {
            this.paramSubscription.unsubscribe();
        }
    }
}
