import { CHANGE_ID } from './gerrit-utils';

const VALID_CHANGE_IDS: string[] = [
    'I2562b857b3461cb75e784fa04140a1c70284202a',
    'I2562b857b3461cb75e784fa041',
    'I2562b857b3461cb75e7',
    'I2562b857b3461',
    'I2562b857'
];
const INVALID_CHANGE_IDS: string[] = ['I256b85', 'I256b'];  // Too short

describe('gerrit-utils_Change-ID_regex', () => {
    VALID_CHANGE_IDS.forEach((cid: string) => {
        it(`gerrit-utils_matches_${cid}`, () => { expect(CHANGE_ID.test(cid)).toBe(true); });
    });

    INVALID_CHANGE_IDS.forEach((cid: string) => {
        it(`gerrit-utils_doesn_t_match_${cid}`, () => {
            expect(CHANGE_ID.test(cid)).toBe(false);
        });
    });
});
