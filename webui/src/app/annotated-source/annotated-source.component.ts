import {
    AfterViewInit, Component, ElementRef, Inject, Input, OnDestroy, OnInit,
    ViewChild
} from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';

import { PageScrollInstance, PageScrollService } from 'ng2-page-scroll';
import { Subscription } from 'rxjs';

import {
    IAnnotatedSourceFile, IAnnotatedSourceMessage, IPropertyFilter, IRuleFilter,
    IToolFilter
} from 'gnat';

import { FilterEvent } from '../filter-selector';

import '../array/operator/sum';

type MessagesByToolId = { [toolId: number]: IAnnotatedSourceMessage[] };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ]
})
export class AnnotatedSourceComponent
    implements AfterViewInit, OnDestroy, OnInit
{
    @Input() public source: IAnnotatedSourceFile = null;
    public selectedLine: number = null;
    public displayMessages: boolean = true;
    public displayCoverage: boolean = true;
    public showMessageList: boolean = true;
    public inlineMessages: { [line: number]: MessagesByToolId } = {};

    private sub: Subscription = null;

    @ViewChild('scrollView') private scrollView: ElementRef;

    constructor(
        @Inject(DOCUMENT) private document: Document,
        private pageScrollService: PageScrollService,
        private route: ActivatedRoute) {}

    /** @override */
    public ngOnInit(): void {
        this.source.coverage = this.source.coverage || {};
        this.filterInlineMessages();

        this.sub = this.route.params.subscribe(params => {
            this.selectedLine = +params['line'];
            this.goToLine(this.selectedLine);
        });
    }

    /** @override */
    public ngAfterViewInit(): void {
        this.goToLine(this.selectedLine);
    }

    /** @override */
    public ngOnDestroy(): void {
        this.sub.unsubscribe();
    }

    /**
     * Checkbox handled for the tool filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onToolFilterToggle(event: FilterEvent) {
        const tool = <IToolFilter> event.option;
        this.source.tools[tool.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /**
     * Checkbox handled for the rule filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onRuleFilterToggle(event: FilterEvent) {
        const rule = <IRuleFilter> event.option;
        this.source.rules[rule.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /**
     * Checkbox handled for the property filters.
     *
     * @param event Event fired by <filter-selector> on checkbox status change.
     */
    public onPropertyFilterToggle(event: FilterEvent) {
        const property = <IPropertyFilter> event.option;
        this.source.properties[property.id]._ui_unselected = !event.checked;
        this.updateMessagesUiProperties();
    }

    /**
     * Update _ui_* properties on inline messages to trigger a refresh.
     */
    private updateMessagesUiProperties() {
        Object.keys(this.source.tools).forEach(id =>
            this.source.tools[id]._ui_selected_message_count = 0);
        Object.keys(this.source.rules).forEach(id =>
            this.source.rules[id]._ui_selected_message_count = 0);
        Object.keys(this.source.properties).forEach(id =>
            this.source.properties[id]._ui_selected_message_count = 0);

        Object.keys(this.source.messages).forEach(line => {
            this.source.messages[line].forEach(message => {
                const tid = message.rule.tool.id;
                const rid = message.rule.id;

                const isToolSelected = !this.source.tools[tid]._ui_unselected;
                const isRuleSelected = !this.source.rules[rid]._ui_unselected;
                const hasSelectedProperties = !message.properties ||
                    !message.properties.length || message.properties.some(
                        p => !this.source.properties[p.id]._ui_unselected);

                message._ui_hidden = !isToolSelected || !isRuleSelected ||
                    !hasSelectedProperties;

                if (!message._ui_hidden) {
                    this.source.tools[tid]._ui_selected_message_count++;
                    this.source.rules[rid]._ui_selected_message_count++;
                    message.properties.forEach(property =>
                        this.source.properties[property.id]
                            ._ui_selected_message_count++);
                }
            });
            this.filterInlineMessages();
        });
    }

    /**
     * (Re)build the ``inlineMessages`` map.
     */
    private filterInlineMessages() {
        for (let line of Object.keys(this.source.messages || {})) {
            const mappedByToolId: MessagesByToolId = {};
            this.source.messages[line]
                .filter(message => !message._ui_hidden)
                .forEach(message => {
                    const toolId = message.rule.tool.id;
                    if (!mappedByToolId.hasOwnProperty(toolId)) {
                        mappedByToolId[toolId] = [];
                    }
                    mappedByToolId[toolId].push(message);
                });
            this.inlineMessages[line] = mappedByToolId;
        }
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    private goToLine(line: number): void {
        let scroll: PageScrollInstance =
            PageScrollInstance.simpleInlineInstance(
                this.document, `#L${line}`, this.scrollView.nativeElement);
        this.pageScrollService.start(scroll);
    };
}
