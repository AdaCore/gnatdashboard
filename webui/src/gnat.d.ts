declare module 'gnat' {
    // Valid values are:
    //
    //  * NO_CODE
    //  * COVERED
    //  * NOT_COVERED
    //  * PARTIALLY_COVERED
    type CoverageStatus = string;

    export interface ISource {
        filename: string;
        metrics?: { [metricId: number]: IGNAThubMetric }[];
        message_count?: { [toolId: number]: number };
        coverage?: number;
    }

    export interface ISourceDir {
        path: string;
        sources: ISource[];
        message_count?: { [toolId: number]: number };
        coverage?: number;
        _ui_expanded?: boolean;
    }

    export interface IProjectModule {
        name: string;
        source_dirs: { [sourceDir: string]: ISourceDir };
        message_count?: { [toolId: number]: number };
        _source_dirs_common_prefix: string;
        _ui_expanded?: boolean;
    }

    export interface IGNAThubReport {
        modules: { [moduleName: string]: IProjectModule };
        project: string;
        creation_time: number;
        tools: Array<{ [id: number]: string }>;
        _database: string;
    }

    export interface IGNAThubTool {
        id: number;
        name: string;
        message_count?: number;
        ui_selected?: boolean;
    }

    export interface IGNAThubRule {
        id: number;
        identifier: string;
        name: string;
        kind: number;
        tool: IGNAThubTool;
        message_count?: number;
        ui_selected?: boolean;
    }

    export interface IGNAThubProperty {
        id: number;
        identifier: string;
        name: string;
        message_count?: number;
        ui_selected?: boolean;
    }

    export interface IGNAThubMetric {
        rule: IGNAThubRule;
        value: string;
    }

    export interface IGNAThubMessage {
        begin: number;
        end: number;
        rule: IGNAThubRule;
        properties: IGNAThubProperty[];
        message: string;
    }

    export interface IGNAThubBlobLine {
        number: number;
        content: string;
        html_content: string;
        coverage: { status: CoverageStatus; hits: number };
        messages: IGNAThubMessage[];
    }

    export interface IGNAThubBlob {
        project: string;
        filename: string;
        source_dir: string;
        full_path: string;
        has_messages: boolean;
        has_coverage: boolean;
        lines: IGNAThubBlobLine[];
        tools: { [id: number]: IGNAThubTool };
        rules: { [id: number]: IGNAThubRule };
        properties: { [id: number]: IGNAThubProperty };
        metrics?: { [metricId: number]: IGNAThubMetric }[];
        message_count?: { [toolId: number]: number };
    }
}
