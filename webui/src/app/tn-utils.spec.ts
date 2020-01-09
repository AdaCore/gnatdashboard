import { TN } from './tn-utils';

const VALID_TNS: string[] = ['OA08-027', 'OB03-043', '1234-456'];
const INVALID_TNS: string[] = ['1234-567'];

describe('tn-utils:TN regex', () => {
    VALID_TNS.forEach((tn: string) => {
        it(`tn-utils: TN regex: matches ${tn}`, () => { expect(TN.test(tn)).toBe(true); });
    });

    INVALID_TNS.forEach((tn: string) => {
        it(`tn-utils: TN regex: doesn't match ${tn}`, () => { expect(TN.test(tn)).toBe(false); });
    });
});
