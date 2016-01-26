declare module "gnat/reports" {

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

    export interface IGNATcoverageHunkStatement {
        coverage: CoverageStatus;
        id: string;             // TODO(delay): should be number
        range: number[][];      // TODO(delay): should define a Range type
        text: string;
    }

    export interface IGNATcoverageHunkMapping {
        coverage: CoverageStatus;
        line: IGNATcoverageHunkLine;
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

}
