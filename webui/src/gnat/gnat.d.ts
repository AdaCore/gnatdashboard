declare module 'gnat' {
    // Valid values are:
    //
    //  * NO_CODE
    //  * COVERED
    //  * NOT_COVERED
    //  * PARTIALLY_COVERED
    type CoverageStatus = string;

    export interface IFileMetrics {
        // Available on all Ada sources
        all_lines: number;
        blank_lines: number;
        code_lines: number;
        comment_lines: number;
        eol_comments: number;
        comment_percentage: number;
        // Available on implementations only (body files)
        cyclomatic_complexity?: number;
        expression_complexity?: number;
        statement_complexity?: number;
        essential_complexity?: number;
        max_loop_nesting?: number;
    }

    export interface ISource {
        filename: string;
        partname: string;
        metrics?: IFileMetrics;
        _associated_resource: boolean;
    }

    export interface IProjectModule {
        [sourceDir: string]: ISource[];
    }

    export interface IGNAThubTool {
        [id: number]: string;
    }

    export interface IGNAThubReport {
        modules: { [moduleName: string]: IProjectModule };
        project: string;
        creation_time: number;
        tools: { [id: number]: string }[];
        _database: string;
    }

    export interface IGNAThubBlobLine {
        no: number;
        content: string;
        coverage: CoverageStatus;
        messages: IGNAThubMessage[];
    }

    export interface IGNAThubRule {
        identifier: string;
        name: string;
        kind: number;
        tool: string;
    }

    export interface IGNAThubMessage {
        begin: number;
        end: number;
        rule: IGNAThubRule;
        message: string;
    }

    export interface IGNAThubBlob {
        project: string;
        filename: string;
        lines: IGNAThubBlobLine[];
        metrics: IGNAThubMessage[];
    }
}
