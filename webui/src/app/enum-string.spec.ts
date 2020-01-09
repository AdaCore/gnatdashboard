import { EnumString } from './enum-string';

const APPLE: string = 'apple';
const ORANGE: string = 'orange';
const PLUM: string = 'plum';

describe('enum-string: EnumString() Enumeration with string values', () => {
    class Fruits extends EnumString {
        public static apple: EnumString = new EnumString(APPLE);
        public static orange: EnumString = new EnumString(ORANGE);
        public static plum: EnumString = new EnumString(PLUM);
    }

    it('enum-string: EnumString() contains strings', () => {
        expect(`${Fruits.apple}`).toEqual(APPLE);
        expect(`${Fruits.orange}`).toEqual(ORANGE);
        expect(`${Fruits.plum}`).toEqual(PLUM);
    });

    it('enum-string: EnumString() can be compared', () => {
        expect(Fruits.apple).toEqual(Fruits.apple);
        expect(Fruits.apple).not.toEqual(Fruits.orange);
        expect(Fruits.apple).not.toEqual(Fruits.plum);
    });
});
