export class PathEncoder {
    /**
     * Extend this class to injected in the template context.
     *
     * @param path The program path to encode to use as an URL parameter.
     * @return The encoded program path.
     */
    public encodePath(path: string): string {
        return encodeURIComponent(path);
    }

    /**
     * Extend this class to injected in the template context.
     *
     * @param path The program path to decode to use as an URL parameter.
     * @return The decoded program path.
     */
    public decodePath(path: string): string {
        return decodeURIComponent(path);
    }
}
