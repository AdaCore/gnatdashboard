declare module 'gnat' {
    // Valid values are:
    //
    //  * NO_CODE
    //  * COVERED
    //  * NOT_COVERED
    //  * PARTIALLY_COVERED
    type CoverageStatus = string;

    export interface ICoverage {
        hits: number;
        status: CoverageStatus;
    }

    export interface IFilter {
        _message_count: number;
        _ui_unselected?: boolean;
        _ui_selected_message_count: number;
    }

    export interface ITool {
        id: number;
        name: string;
    }

    export interface IToolFilter extends ITool, IFilter {}

    export interface IRule {
        id: number;
        identifier: string;
        name: string;
        kind: number;
        tool: ITool;
    }

    export interface IRuleFilter extends IRule, IFilter {}

    export interface IProperty {
        id: number;
        identifier: string;
        name: string;
    }

    export interface IPropertyFilter extends IProperty, IFilter {}

    export interface IMetric {
        rule: IRule;
        value: string;
    }

    // Annotated source file

    export interface IAnnotatedSourceMessage {
        begin: number;
        end: number;
        rule: IRule;
        properties: IProperty[];
        text: string;
        _ui_hidden?: boolean;
    }

    export interface IAnnotatedSourceLine {
        number: number;
        content: string;
        html_content: string;
    }

    export interface IAnnotatedSourceFile {
        project: string;
        filename: string;
        source_dir: string;
        full_path: string;
        has_messages: boolean;
        has_coverage: boolean;
        lines: IAnnotatedSourceLine[];
        tools: { [id: number]: IToolFilter };
        rules: { [id: number]: IRuleFilter };
        properties: { [id: number]: IPropertyFilter };
        metrics?: Array<{ [metricId: number]: IMetric }>;
        coverage?: { [line: number]: ICoverage };
        messages?: { [line: number]: IAnnotatedSourceMessage[] };
        message_count?: { [toolId: number]: number };
    }

    // Index structure

    export interface ISource {
        filename: string;
        metrics?: Array<{ [metricId: number]: IMetric }>;
        coverage?: number;
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _ui_total_message_count?: number;
    }

    export interface ISourceDir {
        path: string;
        sources: ISource[];
        coverage?: number;
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _ui_expanded?: boolean;
    }

    export interface IModule {
        name: string;
        source_dirs: { [sourceDir: string]: ISourceDir };
        coverage?: number;
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _source_dirs_common_prefix: string;
        _ui_expanded?: boolean;
    }

    export interface IReportIndex {
        modules: { [moduleName: string]: IModule };
        project: string;
        creation_time: number;
        tools: { [id: number]: IToolFilter };
        rules: { [id: number]: IRuleFilter };
        properties: { [id: number]: IPropertyFilter };
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _database: string;
    }

}
