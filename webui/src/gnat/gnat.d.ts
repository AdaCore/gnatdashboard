declare module 'gnat' {

    type GNATcoverageLevel = string;
    type CoverageStatus = string;

    // export class GNATcoverageLevel extends EnumString {
    //     static Statement = new EnumString("stmt");
    //     // TODO(delay): complete coverage levels list
    // }
    //
    // export class CoverageStatus extends EnumString {
    //     static NoCode = new EnumString(".");
    //     static Covered = new EnumString("+");
    //     static NotCovered = new EnumString("-");
    //     static PartiallyCovered = new EnumString("!");
    //     static ExemptedNoViolation = new EnumString("#");
    //     static ExemptedWithViolation = new EnumString("*");
    // }

    export interface IGNATcoverageFileStats {
        no_code: number;
        covered: number;
        not_covered: number;
        partially_covered: number;
        exempted_no_violation: number;
        exempted_with_violation: number;
    }

    export interface IGNATcoverageSourceInfo {
        coverage_level: GNATcoverageLevel;
        filename: string;
        hunk_filename: string;
        project: string;
        stats: IGNATcoverageFileStats;
    }

    export interface IGNATcoverageTraceInfo {
        date: string;
        filename: string;
        program: string;
        tag: string;
    }

    export interface IGNATcoverageHunkLine {
        exempted: string;       // TODO(delay): should be boolean
        number: string;         // TODO(delay): should be number
        src: string;
    }

    export interface IGNATcoverageHunkMessage {
        SCO: string;
        kind: string;
        message: string;
    }

    export interface IGNATcoverageHunkStatement {
        coverage: CoverageStatus;
        id: string;             // TODO(delay): should be number
        range: number[][];      // TODO(delay): should define a Range type
        text: string;
    }

    export interface IGNATcoverageHunkMapping {
        coverage: CoverageStatus;
        line: IGNATcoverageHunkLine;
        message?: IGNATcoverageHunkMessage;
        statements?: IGNATcoverageHunkStatement[];
    }

    export interface IGNATcoverageReport {
        coverage_level: GNATcoverageLevel;
        sources: IGNATcoverageSourceInfo[];
        traces: IGNATcoverageTraceInfo[];
    }

    export interface IGNATcoverageHunk {
        coverage_level: GNATcoverageLevel;
        project: string;
        filename: string;
        hunk_filename: string;
        mappings: IGNATcoverageHunkMapping[];
        stats: IGNATcoverageFileStats;
    }

    // WIP

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

    export interface IGNAThubReport {
        project: string;
        _database: string;
        modules: { [moduleName: string]: IProjectModule[] };
    }

    export interface IGNAThubBlobLine {
        no: number;
        content: string;
        coverage: string;
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
