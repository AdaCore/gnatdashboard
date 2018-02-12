import {
    AfterViewInit,
    Component,
    ElementRef,
    HostBinding,
    Inject,
    Input,
    OnDestroy,
    OnInit,
    ViewChild
} from '@angular/core';
import { DOCUMENT } from '@angular/platform-browser';
import { ActivatedRoute } from '@angular/router';

import { PageScrollInstance, PageScrollService } from 'ng2-page-scroll';
import { Observable, Subscription } from 'rxjs';

import {
    IAnnotatedSourceFile,
    IAnnotatedSourceMessage,
    IPropertyFilter,
    IRuleFilter,
    IToolFilter
} from 'gnat';

import { FilterEvent } from '../filter-selector';

type MessagesByToolId = { [toolId: number]: IAnnotatedSourceMessage[] };

@Component({
    selector: 'annotated-source',
    templateUrl: './annotated-source.component.html',
    styleUrls: [ 'annotated-source.component.scss' ]
})
export class AnnotatedSourceComponent
    implements AfterViewInit, OnDestroy, OnInit
{
    @Input() public source: IAnnotatedSourceFile;
    public selectedLine: number = 0;
    public displayMessages: boolean = true; //TODO: check this variable
    @HostBinding('class.with-coverage') public displayCoverage: boolean = true;
    public showMessageList: boolean = true;
    public inlineMessages: { [line: number]: MessagesByToolId };
    public inlineMessagesShownCount: number = 0;

    private sub: Subscription;

    @ViewChild('scrollView') private scrollView: ElementRef;

    constructor(
        @Inject(DOCUMENT) private document: Document,
        private pageScrollService: PageScrollService,
        private route: ActivatedRoute) {}

    /** @override */
    public ngOnInit() {
        this.source.coverage = this.source.coverage || {};
        this.source.messages = this.source.messages || {};
        this.source.rules = this.source.rules || {};
        this.source.tools = this.source.tools || {};
        this.source.properties = this.source.properties || {};

        // Prepare inline messages.
        this.inlineMessages = {};
        Object.keys(this.source.messages).forEach(line => {
            this.source.messages[line].forEach(message => {
                const toolId = message.rule.tool.id;
                if (!this.inlineMessages.hasOwnProperty(line)) {
                    this.inlineMessages[line] = {};
                }
                if (!this.inlineMessages[line].hasOwnProperty(toolId)) {
                    this.inlineMessages[line][toolId] = new Set();
                }
                this.inlineMessages[line][toolId].add(message);
                this.inlineMessagesShownCount++;
            });
        });

        this.sub = this.route.params.subscribe(params => {
            this.selectedLine = +params['line'];
            this.goToLine(this.selectedLine);
        });
    }

    /** @override */
    public ngAfterViewInit() {
        this.goToLine(this.selectedLine);
    }

    /** @override */
    public ngOnDestroy() {
        if (this.sub) {
            this.sub.unsubscribe();
        }
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
        this.inlineMessagesShownCount = 0;

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
                    this.inlineMessagesShownCount++;
                    this.insertIfMissing(line, message);
                } else {
                    this.removeIfPresent(line, message);
                }
            });
        });
    }

    private insertIfMissing(line: string, message: IAnnotatedSourceMessage) {
        const toolId = message.rule.tool.id;
        if (!this.inlineMessages.hasOwnProperty(line)) {
            this.inlineMessages[line] = {};
        }
        if (!this.inlineMessages[line].hasOwnProperty(toolId)) {
            this.inlineMessages[line][toolId] = new Set();
        }
        this.inlineMessages[line][toolId].add(message);
    }

    private removeIfPresent(line: string, message: IAnnotatedSourceMessage) {
        const toolId = message.rule.tool.id;
        if (!this.inlineMessages.hasOwnProperty(line) ||
            !this.inlineMessages[line].hasOwnProperty(toolId)) {
            return; // Message not found.
        }
        this.inlineMessages[line][toolId].delete(message);
        if (!this.inlineMessages[line][toolId].size) {
            // Delete the list when the last message is removed.
            delete this.inlineMessages[line][toolId];
            if (!Object.keys(this.inlineMessages[line]).length) {
                // Delete the map when last message is removed.
                delete this.inlineMessages[line];
            }
        }
    }

    /**
     * Scroll into the source file up to a given line.
     *
     * @param line The number of the line to scroll to.
     */
    private goToLine(line: number) {
        if (line) {
            let scroll: PageScrollInstance =
                PageScrollInstance.simpleInlineInstance(
                    this.document, `#L${line}`, this.scrollView.nativeElement);
            this.pageScrollService.start(scroll);
        }
    };
}
