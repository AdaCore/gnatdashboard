import { TN } from './tn-utils';

const VALID_TNS: string[] = ['OA08-027', 'OB03-043', '1234-456'];
const INVALID_TNS: string[] = ['1234-567'];

describe('TN regex', () => {
    VALID_TNS.forEach((tn: string) => {
        it(`matches ${tn}`, () => { expect(TN.test(tn)).toBe(true); });
    });

    INVALID_TNS.forEach((tn: string) => {
        it(`doesn't match ${tn}`, () => { expect(TN.test(tn)).toBe(false); });
    });
});
