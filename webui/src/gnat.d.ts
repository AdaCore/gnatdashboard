declare module 'gnat' {
    // Valid values are:
    //
    //  * NO_CODE
    //  * COVERED
    //  * NOT_COVERED
    //  * PARTIALLY_COVERED
    type CoverageStatus = string;

    export interface ICoverage {
        char: string;
        status: CoverageStatus;
    }

    // **
    //  Part for the filter.json (filter-panel)
    // **

    // Create the property for any filter category
    export interface IFilter {
        _message_count: number;
        _ui_unselected?: boolean;
        _ui_selected_message_count?: number;
    }

    // Create the property for the tool filter
    export interface ITool {
        id: number;
        name: string;
    }

    // Create the property for the rule filter
    export interface IRule {
        id: number;
        name: string;
        tool_id: number;
    }

    // Create the property for the property filter
    export interface IProperty {
        id: number;
        name: string;
        tool_id: number;
    }

    // Create the property for the ranking filter
    export interface IRanking {
        id: number;
        name: string;
        tool_id: number;
    }

    // Create the review for the review filter
    export interface IReview {
        id: number;
        name: string;
        display_name: string;
        kind: string;
    }

    // Now concats the propert for filters and for specific filter
    export interface IRuleFilter extends IRule, IFilter {}
    export interface IToolFilter extends ITool, IFilter {}
    export interface IPropertyFilter extends IProperty, IFilter {}
    export interface IRankingFilter extends IRanking, IFilter {}
    export interface IReviewFilter extends IReview, IFilter {}

    // Define the model for the filter object
    export interface IFilterIndex {
        project: string;
        creation_time: number;
        tools: [ IToolFilter ];
        rules: [ IRuleFilter ];
        properties: [ IPropertyFilter ];
        ranking: [ IRankingFilter ];
        review_status: [ IReviewFilter ];
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _database: string;
    }

    // **
    //  Part for the code.json (code-navigation)
    // **

    export interface ISource {
        filename: string;
        coverage?: number;
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _ui_total_message_count?: number;
        expand?: boolean;
    }

    export interface ISourceDir {
        name: string;
        message_count?: { [toolId: number]: number };
        coverage?: number;
        _total_message_count: number;
        _ui_total_message_count?: number;
        sources: ISource[];
        expand?: boolean;
    }

    export interface IModule {
        name: string;
        message_count?: { [toolId: number]: number };
        _total_message_count: number;
        _ui_total_message_count?: number;
        _source_dirs_common_prefix: string;
        coverage?: number;
        source_dirs: ISourceDir[];
        expand?: boolean;
    }

    export interface ICodeIndex {
        modules: IModule[];
    }

    // **
    //  Part for the message.json (message-navigation)
    // **

    export interface ICountRanking {
        High: number;
        Medium: number;
        Low: number;
        Info: number;
        Unspecified: number;
    }

    export interface IReviewUser {
        author: string;
        status: string;
        status_priority: number;
        status_kind: string;
        date: string;
        from_source: string;
        message: string;
        display_name: string;
    }

    export interface IMessage {
        id: number;
        col_begin: number;
        col_end: number;
        line: number;
        name: string;
        status_priority: number;
        properties: [IProperty];
        rule: IRule;
        ranking: IRanking;
        tool_msg_id: number;
        tool: string;
        user_review: IReviewUser;
        review_history: IReviewUser[];
        hide?: boolean;
    }

    export interface ISourceNav {
        filename: string;
        source_dir: string;
        full_path: string;
        coverage?: number;
        message_count?: { [toolId: number]: number };
        messages?: IMessage[];
        annotations?: [IMessage];
        countRanking?: ICountRanking;
        _total_message_count: number;
        _ui_total_message_count?: number;
        expand?: boolean;
    }

    export interface IMessageIndex {
        sources: ISourceNav[];
        _ui_total_message_count?: number;
    }

    // **
    //  Part for the FILENAME.json (annotated-source)
    // **

    export interface IMetric {
        rule: IRule;
        value: string;
    }

    // Annotated source file

    export interface IAnnotatedSourceMessage {
        begin: number;
        end: number;
        rule: IRule;
        line: number;
        properties: [IProperty];
        name: string;
        _ui_hidden?: boolean;
    }

    export interface IAnnotatedSourceLine {
        number: number;
        content: string;
    }

    export interface IAnnotatedSourceFile {
        project: string;
        filename: string;
        source_dir: string;
        full_path: string;
        lines: [IAnnotatedSourceLine];
        metrics?: Array<IMetric>;
        coverage?: { [line: number]: ICoverage };
        messages?: IAnnotatedSourceMessage[];
        annotations?: IAnnotatedSourceMessage[];
    }

    // Interface for the sorting instance
    export interface ISort {
        otherSort: string;
        newSort: string;
        order?: number;
    }

}
