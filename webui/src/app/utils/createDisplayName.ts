
export function createDisplayName(name: string): string {
    let myArray: string[] = name.split('_');
    let displayName: string = name.split('_').map(function(word: string): string {
        return word.charAt(0).toUpperCase() + word.slice(1).toLowerCase();
    }).join(' ');
    return displayName;
}
