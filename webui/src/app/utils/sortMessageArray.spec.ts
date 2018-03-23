import { sortMessageArray } from './sortArray';
import * as _ from 'lodash';

const message = {
    sources: [
        {
            filename: 'code1.adb',
            source_dir: 'my_project',
            full_path: '/my_dir/my_project/code1.adb',
            coverage: 79,
            message_count: {
                3: 5,
                5: 5
            },
            messages: [
                {
                    line: 10,
                    col_begin: 30,
                    col_den: 30,
                    name: 'precondition1',
                    value: 'precondition : this is an error',
                    id: 100
                }, {
                    line: 25,
                    col_begin: 60,
                    col_den: 60,
                    name: 'precondition2',
                    value: 'precondition : this is a second error',
                    id: 101
                }
            ],
            _total_message_count: 16,
            _ui_total_message_count: 0
        }, {
            filename: 'code2.adb',
            source_dir: 'my_project',
            full_path: '/my_dir/my_project/code2.adb',
            coverage: 79,
            message_count: {
                3: 5
            },
            messages: [
                {
                    line: 10,
                    col_begin: 30,
                    col_den: 30,
                    name: 'precondition2',
                    value: 'precondition : this is an error',
                    id: 100
                }, {
                    line: 25,
                    col_begin: 60,
                    col_den: 60,
                    name: 'precondition1',
                    value: 'precondition : this is a second error',
                    id: 101
                }
            ],
            _total_message_count: 15,
            _ui_total_message_count: 0
        }
    ]
};

describe('sortMessageArray()', () => {

    const oldFilter = {newSort: 'name', otherSort: 'filename', order: -1};

    // ascendent name
    let newFilter = {newSort: 'name', otherSort: 'filename'};
    const testSort1 = sortMessageArray(newFilter, oldFilter,
                                       _.cloneDeep(message.sources));
    it('sortMessageArray() by name/filename ascendant', () => {
        expect(testSort1[0].filename).toEqual('code1.adb');
        expect(testSort1[0].messages[0].name).toEqual('precondition1');
        expect(testSort1[0].messages[1].name).toEqual('precondition2');
        expect(testSort1[1].filename).toEqual('code2.adb');
        expect(testSort1[1].messages[0].name).toEqual('precondition1');
        expect(testSort1[1].messages[1].name).toEqual('precondition2');
    });

    // descendent name
    newFilter = {newSort: 'name', otherSort: 'filename'};
    const testSort2 = sortMessageArray(newFilter, oldFilter,
                                       _.cloneDeep(message.sources));
    it('sortMessageArray() by name/filename descendant', () => {
        expect(testSort2[0].filename).toEqual('code2.adb');
        expect(testSort2[0].messages[0].name).toEqual('precondition2');
        expect(testSort2[0].messages[1].name).toEqual('precondition1');
        expect(testSort2[1].filename).toEqual('code1.adb');
        expect(testSort2[1].messages[0].name).toEqual('precondition2');
        expect(testSort2[1].messages[1].name).toEqual('precondition1');
    });

    // ascendent message_count
    newFilter = {newSort: '_total_message_count', otherSort: 'line'};
    const testSort3 = sortMessageArray(newFilter, oldFilter,
                                       _.cloneDeep(message.sources));

    it('sortMessageArray() by _total_message_count/line ascendant', () => {
        expect(testSort3[0]._total_message_count).toEqual(15);
        expect(testSort3[0].messages[0].line).toEqual(10);
        expect(testSort3[0].messages[1].line).toEqual(25);
        expect(testSort3[1]._total_message_count).toEqual(16);
        expect(testSort3[1].messages[0].line).toEqual(10);
        expect(testSort3[1].messages[1].line).toEqual(25);
    });

    // ascendent message_count
    newFilter = {newSort: '_total_message_count', otherSort: 'line'};
    const testSort4 = sortMessageArray(newFilter, oldFilter,
                                       _.cloneDeep(message.sources));

    it('sortMessageArray() by _total_message_count/line descendant', () => {
        expect(testSort4[0]._total_message_count).toEqual(16);
        expect(testSort4[0].messages[0].line).toEqual(25);
        expect(testSort4[0].messages[1].line).toEqual(10);
        expect(testSort4[1]._total_message_count).toEqual(15);
        expect(testSort4[1].messages[0].line).toEqual(25);
        expect(testSort4[1].messages[1].line).toEqual(10);
    });

    // test with full filter on, same as the old one
    let completeFilter = oldFilter;
    const testSort5 = sortMessageArray(completeFilter, oldFilter,
                                       _.cloneDeep(message.sources));
    it('sortMessageArray() only refresh', () => {
        expect(testSort5[0]._total_message_count).toEqual(16);
        expect(testSort5[0].messages[0].line).toEqual(25);
        expect(testSort5[0].messages[1].line).toEqual(10);
        expect(testSort5[1]._total_message_count).toEqual(15);
        expect(testSort5[1].messages[0].line).toEqual(25);
        expect(testSort5[1].messages[1].line).toEqual(10);
    });

    // test with full filter on, a whle new filter
    completeFilter = {newSort: 'name', otherSort: 'filename', order: -1};
    const testSort6 = sortMessageArray(completeFilter, oldFilter,
                                       _.cloneDeep(message.sources));
    it('sortMessageArray() whole newFilter', () => {
        expect(testSort6[0].filename).toEqual('code2.adb');
        expect(testSort6[0].messages[0].name).toEqual('precondition2');
        expect(testSort6[0].messages[1].name).toEqual('precondition1');
        expect(testSort6[1].filename).toEqual('code1.adb');
        expect(testSort6[1].messages[0].name).toEqual('precondition2');
        expect(testSort6[1].messages[1].name).toEqual('precondition1');
    });

});
