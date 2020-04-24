import { TN } from './tn-utils';

const VALID_TNS: string[] = ['OA08-027', 'OB03-043', '1234-456'];
const INVALID_TNS: string[] = ['1234-567'];

describe('tn-utils_TN_regex', () => {
    VALID_TNS.forEach((tn: string) => {
        it(`tn-utils: TN regex: matches_${tn}`, () => { expect(TN.test(tn)).toBe(true); });
    });

    INVALID_TNS.forEach((tn: string) => {
        it(`tn-utils_TN_regex_doesn_t_match_${tn}`, () => { expect(TN.test(tn)).toBe(false); });
    });
});
