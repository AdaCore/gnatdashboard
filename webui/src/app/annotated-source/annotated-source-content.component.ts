import {
    Component,
    Input,
    OnDestroy,
    OnInit
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import { AnnotatedSourceComponent } from './annotated-source.component'

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
    @Input() public inlineAnnotations: { [line: number]: MessagesByToolId };
    @Input() public displayMessages;

    public selectedLine: number;
    private paramSubscription: Subscription;

    constructor(private route: ActivatedRoute,
                 private source: AnnotatedSourceComponent) {}

    public ngOnInit() {
        this.paramSubscription = this.route.params.subscribe(params => {
            if (+params['line']){
                this.selectedLine = +params['line'];
            }
        });
    }

    public ngOnDestroy() {
        if (this.paramSubscription) {
            this.paramSubscription.unsubscribe();
        }
    }

    public trackLine(index, line){
        return line ? line.number : undefined;
    }
}
