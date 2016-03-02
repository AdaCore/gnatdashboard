/**
 * Enumeration-like class working with string values.
 *
 * This comes in handy to describe a fixed set of values.
 */
export class EnumString {
    constructor(public value: string) { }

    /**
     * @return The string representation of the enumeration.
     * @override
     */
    public toString(): string {
        return this.value;
    }
}
