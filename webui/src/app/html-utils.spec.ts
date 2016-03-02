import { unescapeHTML } from './html-utils';

describe('Unescape HTML', () => {
    it('convert into expected HTML', () => {
        expect(unescapeHTML('&gt;')).toEqual('>');
        expect(unescapeHTML('&lt;')).toEqual('<');
        expect(unescapeHTML('&lt;&amp;&gt;')).toEqual('<&>');
        expect(unescapeHTML('&quot;')).toEqual('"');
        expect(unescapeHTML('&apos;')).toEqual('\'');
    });
});

