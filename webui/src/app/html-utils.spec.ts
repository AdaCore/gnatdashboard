import { unescapeHTML } from './html-utils';

describe('Unescape_HTML', () => {
    it('html-utils_unescapeHTML_convert_into_expected_HTML', () => {
        expect(unescapeHTML('&gt;')).toEqual('>');
        expect(unescapeHTML('&lt;')).toEqual('<');
        expect(unescapeHTML('&lt;&amp;&gt;')).toEqual('<&>');
        expect(unescapeHTML('&quot;')).toEqual('"');
        expect(unescapeHTML('&apos;')).toEqual('\'');
        expect(unescapeHTML('')).toEqual('');
    });
});
