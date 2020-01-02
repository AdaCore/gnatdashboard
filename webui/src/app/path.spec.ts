import { commonprefix } from './path';

type CommonPrefixTestCase = { input: string[], expected: string };
const COMMON_PREFIXES: CommonPrefixTestCase[] = [
    {
        input: ['interspecies', 'interstelar', 'interstate'],
        expected: 'inters'
    },
    {
        input: ['interspec', 'intfoo', 'interspace'],
        expected: 'int'
    },
    {
        input: ['throne', 'throne'],
        expected: 'throne'
    },
    {
        input: ['throne', 'dungeon'],
        expected: ''
    },
    {
        input: ['cheese'],
        expected: 'cheese'
    },
    {
        input: [],
        expected: ''
    },
    {
        input: ['prefix', 'suffix'],
        expected: ''
    }
];

describe('path.commonprefix', () => {
    COMMON_PREFIXES.forEach((tc: CommonPrefixTestCase) => {
        const humanFriendlyExpected: string =
            tc.expected ? `"${tc.expected}"` : 'the empty string';
        it(`returns ${humanFriendlyExpected}`, () => {
            expect(commonprefix(tc.input)).toEqual(tc.expected);
        });
    });
});
