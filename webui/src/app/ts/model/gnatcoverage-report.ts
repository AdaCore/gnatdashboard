import { EnumString } from "../enum-string";

export class GNATcoverageLevel extends EnumString {
    static Statement = new EnumString("stmt");
    // TODO(delay): complete coverage levels list
}

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

export interface IGNATcoverageReport {
    coverage_level: GNATcoverageLevel;
    sources: IGNATcoverageSourceInfo[];
    traces: IGNATcoverageTraceInfo[];
}
