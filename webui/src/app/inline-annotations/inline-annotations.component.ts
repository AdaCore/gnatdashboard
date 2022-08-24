import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { IAnnotatedSourceMessage, ITool } from 'gnat';
import { SharedReport } from '../main-responder.service';

type InlineAnnotations = IAnnotatedSourceMessage[];

@Component({
    selector: 'inline-annotations',
    templateUrl: 'inline-annotations.component.html',
    styleUrls: [ 'inline-annotations.component.scss' ]
})
export class InlineAnnotationsComponent implements OnChanges {
    @Input() public annotations: IAnnotatedSourceMessage[];
    @Input() public line: number;

    constructor( public reportService: SharedReport) {
    }

    public formatAnnotationProperties(annotation: IAnnotatedSourceMessage): string {
        return annotation.properties.map(prop => prop.name).join(', ');
    }

    public openClose(line: number): void {
        let elem: HTMLElement = document.getElementById(line.toString());
        elem.classList.toggle('expand');
    }

    public trackAnnotation(index: number, annotation: IAnnotatedSourceMessage): number {
        return index;
        // return annotation ? annotation.id : undefined;
    }

    public ngOnChanges(changes: SimpleChanges): void {
        if (changes["line"] && !changes["line"].firstChange){
            this.line = changes["line"].currentValue;
        }else if (changes["annotations"] && !changes["annotations"].firstChange){
            this.annotations = changes["annotations"].currentValue;
        }
    }
}
