export function unescapeHTML(html: string): string {
    "use strict";
    const el: HTMLElement = document.createElement("div");
    el.innerHTML = html;
    return el.childNodes.length === 0 ? "" : el.childNodes[0].nodeValue;
}
