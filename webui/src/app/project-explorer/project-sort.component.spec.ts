import { sortMapInArray, sortBy } from './project-sort.component';

const myApple = {filename: 'apple',
                 _total_message_count: 20,
                 _ui_total_message_count:  5};
const myOrange = {filename: 'orange',
                  _total_message_count: 2,
                  _ui_total_message_count:  2};
const myPlum = {filename: 'plum',
                _total_message_count: 15,
                _ui_total_message_count:  10};

const myBanana = {filename: 'banana',
                  _total_message_count: 20};
const myKiwi = {filename: 'kiwi',
                _total_message_count: 10};
const myPeach = {filename: 'peach',
                 _total_message_count: 5};

const MyModule1 = {apple : myApple,
                   orange : myOrange,
                   plum : myPlum};

const MyModule2 = {banana : myBanana,
                   kiwi : myKiwi,
                   peach : myPeach};

describe('sortMapInArray/sortBy', () => {

    const oldFilter = {newSort: 'filename', otherSort: '', order: -1};
    let newFilter = {newSort: 'filename', otherSort: ''};
    const testSortMap1 = sortMapInArray(newFilter, oldFilter, MyModule1);
    // should give [apple, orange, plum]
    newFilter = {newSort: 'filename', otherSort: ''};
    const testSortMap2 = sortMapInArray(newFilter, oldFilter, MyModule2);
    // should give [peach, kiwi, banana]

    it('sortMapInArray() by filename ascendant', () => {
        expect(testSortMap1[0]).toEqual(myApple);
        expect(testSortMap1[1]).toEqual(myOrange);
        expect(testSortMap1[2]).toEqual(myPlum);
    });
    it('sortMapInArray() by filename descendant', () => {
        expect(testSortMap2[0]).toEqual(myPeach);
        expect(testSortMap2[1]).toEqual(myKiwi);
        expect(testSortMap2[2]).toEqual(myBanana);
    });

    newFilter = {newSort: '_ui_total_message_count', otherSort: '_total_message_count'};
    const testSortMap3 = sortMapInArray(newFilter, oldFilter, MyModule1);
    // should give [orange, apple, plum]
    newFilter = {newSort: '_ui_total_message_count', otherSort: '_total_message_count'};
    const testSortMap4 = sortMapInArray(newFilter, oldFilter, MyModule2);
    // should give [banana, kiwi, peach]

    it('sortMapInArray() by _ui_total_message_count/_total_message_count ascendant', () => {
        expect(testSortMap3[0]).toEqual(myOrange);
        expect(testSortMap3[1]).toEqual(myApple);
        expect(testSortMap3[2]).toEqual(myPlum);
    });
    it('sortMapInArray() by _ui_total_message_count/_total_message_count descendant', () => {
        expect(testSortMap4[0]).toEqual(myBanana);
        expect(testSortMap4[1]).toEqual(myKiwi);
        expect(testSortMap4[2]).toEqual(myPeach);
    });

    let completeFilter = oldFilter;
    const testSortMap5 = sortMapInArray(completeFilter, oldFilter, MyModule2);
    // should stay [banana, kiwi, peach]

    it('sortMapInArray() only refresh', () => {
        expect(testSortMap5[0]).toEqual(myBanana);
        expect(testSortMap5[1]).toEqual(myKiwi);
        expect(testSortMap5[2]).toEqual(myPeach);
    });

    completeFilter = {newSort: '_total_message_count', otherSort: '', order: 1};
    const testSortMap6 = sortMapInArray(completeFilter, oldFilter, MyModule1);
    // should be [orange, plum, apple]

    it('sortMapInArray() whole newFilter', () => {
        expect(testSortMap6[0]).toEqual(myOrange);
        expect(testSortMap6[1]).toEqual(myPlum);
        expect(testSortMap6[2]).toEqual(myApple);
        expect(oldFilter.newSort).toEqual('_total_message_count');
        expect(oldFilter.otherSort).toEqual('');
        expect(oldFilter.order).toEqual(1);
    });

});
