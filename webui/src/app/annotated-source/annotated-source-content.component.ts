import {
    Component,
    Input,
    OnDestroy,
    OnInit,
    SimpleChanges,
    OnChanges
} from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';

import { AnnotatedSourceComponent } from './annotated-source.component';

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
export class AnnotatedSourceContentComponent implements OnDestroy, OnInit, OnChanges {
    @Input() public lines: IAnnotatedSourceLine[];
    @Input() public tools: { [toolId: number]: ITool };
    @Input() public coverage: { [line: number]: ICoverage };
    @Input() public inlineAnnotations: { [line: number]: MessagesByToolId };
    @Input() public displayMessages: any[];

    public selectedLine: number;
    private paramSubscription: Subscription;

    constructor(private route: ActivatedRoute,
                private source: AnnotatedSourceComponent) {}

    public ngOnInit(): void {
        this.paramSubscription = this.route.params.subscribe(params => {
            if (+params['line']){
                this.selectedLine = +params['line'];
            }
        });
    }

    public ngOnChanges(changes: SimpleChanges): void {

        if (changes.lines && !changes.lines.firstChange){
            this.lines = changes.lines.currentValue;
        }else if (changes.tools && !changes.tools.firstChange){
            this.tools = changes.tools.currentValue;
        }else if (changes.inlineAnnotations && !changes.inlineAnnotations.firstChange){
            this.inlineAnnotations = changes.inlineAnnotations.currentValue;
        }else if (changes.displayMessages && !changes.displayMessages.firstChange){
            this.displayMessages = changes.displayMessages.currentValue;
        }else if (changes.coverage && !changes.coverage.firstChange){
            this.coverage = changes.coverage.currentValue;
        }

    }

    public ngOnDestroy(): void {
        if (this.paramSubscription) {
            this.paramSubscription.unsubscribe();
        }
    }

    public trackLine(index: number, line: any): number {
        return line ? line.number : undefined;
    }
}
