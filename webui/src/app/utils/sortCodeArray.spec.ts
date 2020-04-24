import { sortCodeArray } from './sortArray';
import { ICodeIndex, ISort, IModule } from 'gnat';
import * as _ from 'lodash';

const code: ICodeIndex = {
    modules: [
        {
            name: 'my_project1',
            message_count: {
                3: 20,
                5: 10
            },
            _total_message_count: 30,
            _source_dirs_common_prefix: '',
            coverage: 76,
            source_dirs: [
                {
                    message_count: {
                        3: 10,
                        5: 5
                    },
                    coverage: 79,
                    name: 'my_folder1',
                    _total_message_count: 15,
                    sources: [
                        {
                            filename: 'code1.adb',
                            coverage: 79,
                            message_count: {
                                3: 5,
                                5: 5
                            },
                            _total_message_count: 10
                        }, {
                            filename: 'code2.adb',
                            coverage: 79,
                            message_count: {
                                3: 5
                            },
                            _total_message_count: 5
                        }
                    ]
                }, {
                    message_count: {
                        3: 10,
                        5: 5
                    },
                    coverage: 79,
                    name: 'my_folder2',
                    _total_message_count: 15,
                    sources: [
                        {
                            filename: 'code1.adb',
                            coverage: 79,
                            message_count: {
                                3: 5,
                                5: 5
                            },
                            _total_message_count: 10
                        }, {
                            filename: 'code2.adb',
                            coverage: 79,
                            message_count: {
                                3: 5
                            },
                            _total_message_count: 5
                        }
                    ]
                }
            ]
        }, {
            name: 'my_project2',
            message_count: {
                3: 12,
                5: 15
            },
            _total_message_count: 27,
            _source_dirs_common_prefix: '',
            coverage: 76,
            source_dirs: [
                {
                    message_count: {
                        3: 10,
                        5: 5
                    },
                    coverage: 79,
                    name: 'my_folder1',
                    _total_message_count: 15,
                    sources: [
                        {
                            filename: 'code1.adb',
                            coverage: 79,
                            message_count: {
                                3: 5,
                                5: 5
                            },
                            _total_message_count: 10
                        }, {
                            filename: 'code2.adb',
                            coverage: 79,
                            message_count: {
                                3: 5
                            },
                            _total_message_count: 5
                        }
                    ]
                }, {
                    message_count: {
                        3: 2,
                        5: 10
                    },
                    coverage: 79,
                    name: 'my_folder2',
                    _total_message_count: 12,
                    sources: [
                        {
                            filename: 'code1.adb',
                            coverage: 79,
                            message_count: {
                                3: 2,
                                5: 5
                            },
                            _total_message_count: 7
                        }, {
                            filename: 'code2.adb',
                            coverage: 79,
                            message_count: {
                                3: 5
                            },
                            _total_message_count: 5
                        }
                    ]
                }
            ]
        }
    ]
};

describe('sortCodeArray', () => {

    const oldFilter: ISort = {newSort: 'name', otherSort: 'filename', order: -1};

    // ascendent name
    let newFilter: ISort = {newSort: 'name', otherSort: 'filename'};
    const testSortCode1: IModule[] = sortCodeArray(newFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray_by_name_filename_ascendant', () => {
        expect(testSortCode1[0].name).toEqual('my_project1');
        expect(testSortCode1[0].source_dirs[0].name).toEqual('my_folder1');
        expect(testSortCode1[0].source_dirs[0].sources[0].filename).toEqual('code1.adb');
        expect(testSortCode1[0].source_dirs[0].sources[1].filename).toEqual('code2.adb');
        expect(testSortCode1[0].source_dirs[1].name).toEqual('my_folder2');
        expect(testSortCode1[0].source_dirs[1].sources[0].filename).toEqual('code1.adb');
        expect(testSortCode1[0].source_dirs[1].sources[1].filename).toEqual('code2.adb');
        expect(testSortCode1[1].name).toEqual('my_project2');
        expect(testSortCode1[1].source_dirs[0].name).toEqual('my_folder1');
        expect(testSortCode1[1].source_dirs[0].sources[0].filename).toEqual('code1.adb');
        expect(testSortCode1[1].source_dirs[0].sources[1].filename).toEqual('code2.adb');
        expect(testSortCode1[1].source_dirs[1].name).toEqual('my_folder2');
        expect(testSortCode1[1].source_dirs[1].sources[0].filename).toEqual('code1.adb');
        expect(testSortCode1[1].source_dirs[1].sources[1].filename).toEqual('code2.adb');
    });

    // descendent name
    newFilter = {newSort: 'name', otherSort: 'filename'};
    const testSortCode2: IModule[] = sortCodeArray(newFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray_by_name_filename_descendant', () => {
        expect(testSortCode2[0].name).toEqual('my_project2');
        expect(testSortCode2[0].source_dirs[0].name).toEqual('my_folder2');
        expect(testSortCode2[0].source_dirs[0].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode2[0].source_dirs[0].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode2[0].source_dirs[1].name).toEqual('my_folder1');
        expect(testSortCode2[0].source_dirs[1].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode2[0].source_dirs[1].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode2[1].name).toEqual('my_project1');
        expect(testSortCode2[1].source_dirs[0].name).toEqual('my_folder2');
        expect(testSortCode2[1].source_dirs[0].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode2[1].source_dirs[0].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode2[1].source_dirs[1].name).toEqual('my_folder1');
        expect(testSortCode2[1].source_dirs[1].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode2[1].source_dirs[1].sources[1].filename).toEqual('code1.adb');
    });

    // ascendent message_count
    newFilter = {newSort: '_ui_total_message_count', otherSort: '_total_message_count'};
    const testSortCode3: IModule[] = sortCodeArray(newFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray_by_ui_total_message_count_total_message_count_ascendant', () => {
        expect(testSortCode3[0]._total_message_count).toEqual(27);

        expect(testSortCode3[0].source_dirs[0]._total_message_count).toEqual(12);

        expect(testSortCode3[0].source_dirs[0].sources[0]._total_message_count)
            .toEqual(5);
        expect(testSortCode3[0].source_dirs[0].sources[1]._total_message_count)
            .toEqual(7);

        expect(testSortCode3[0].source_dirs[1]._total_message_count).toEqual(15);
        expect(testSortCode3[0].source_dirs[1].sources[0]._total_message_count)
            .toEqual(5);
        expect(testSortCode3[0].source_dirs[1].sources[1]._total_message_count)
            .toEqual(10);

        expect(testSortCode3[1]._total_message_count).toEqual(30);
        expect(testSortCode3[1].source_dirs[0]._total_message_count).toEqual(15);
        expect(testSortCode3[1].source_dirs[0].sources[0]._total_message_count)
            .toEqual(5);
        expect(testSortCode3[1].source_dirs[0].sources[1]._total_message_count)
            .toEqual(10);
        expect(testSortCode3[1].source_dirs[1]._total_message_count).toEqual(15);
        expect(testSortCode3[1].source_dirs[1].sources[0]._total_message_count)
            .toEqual(5);
        expect(testSortCode3[1].source_dirs[1].sources[1]._total_message_count)
            .toEqual(10);
    });

    // ascendent message_count
    newFilter = {newSort: '_ui_total_message_count', otherSort: '_total_message_count'};
    const testSortCode4: IModule[] = sortCodeArray(newFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray__ui_total_message_count_total_message_count_descendant', () => {
        expect(testSortCode4[0]._total_message_count).toEqual(30);
        expect(testSortCode4[0].source_dirs[0]._total_message_count)
            .toEqual(15);
        expect(testSortCode4[0].source_dirs[0].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode4[0].source_dirs[0].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode4[0].source_dirs[1]._total_message_count).toEqual(15);
        expect(testSortCode4[0].source_dirs[1].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode4[0].source_dirs[1].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode4[1]._total_message_count).toEqual(27);
        expect(testSortCode4[1].source_dirs[0]._total_message_count).toEqual(15);
        expect(testSortCode4[1].source_dirs[0].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode4[1].source_dirs[0].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode4[1].source_dirs[1]._total_message_count).toEqual(12);
        expect(testSortCode4[1].source_dirs[1].sources[0]._total_message_count)
            .toEqual(7);
        expect(testSortCode4[1].source_dirs[1].sources[1]._total_message_count)
            .toEqual(5);
    });

    // test with full filter on, same as the old one
    let completeFilter: ISort = oldFilter;
    const testSortCode5: IModule[] = sortCodeArray(completeFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray_only_refresh', () => {
        expect(testSortCode5[0]._total_message_count).toEqual(30);
        expect(testSortCode5[0].source_dirs[0]._total_message_count)
            .toEqual(15);
        expect(testSortCode5[0].source_dirs[0].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode5[0].source_dirs[0].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode5[0].source_dirs[1]._total_message_count).toEqual(15);
        expect(testSortCode5[0].source_dirs[1].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode5[0].source_dirs[1].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode5[1]._total_message_count).toEqual(27);
        expect(testSortCode5[1].source_dirs[0]._total_message_count).toEqual(15);
        expect(testSortCode5[1].source_dirs[0].sources[0]._total_message_count)
            .toEqual(10);
        expect(testSortCode5[1].source_dirs[0].sources[1]._total_message_count)
            .toEqual(5);
        expect(testSortCode5[1].source_dirs[1]._total_message_count).toEqual(12);
        expect(testSortCode5[1].source_dirs[1].sources[0]._total_message_count)
            .toEqual(7);
        expect(testSortCode5[1].source_dirs[1].sources[1]._total_message_count)
            .toEqual(5);
    });

    // test with full filter on, with a whole new filter
    completeFilter = {newSort: 'name', otherSort: 'filename', order: -1};
    const testSortCode6: IModule[] = sortCodeArray(completeFilter, oldFilter,
                                        _.cloneDeep(code.modules));
    it('sortCodeArray_whole_newFilter', () => {
        expect(testSortCode6[0].name).toEqual('my_project2');
        expect(testSortCode6[0].source_dirs[0].name).toEqual('my_folder2');
        expect(testSortCode6[0].source_dirs[0].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode6[0].source_dirs[0].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode6[0].source_dirs[0].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode6[0].source_dirs[1].name).toEqual('my_folder1');
        expect(testSortCode6[0].source_dirs[1].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode6[0].source_dirs[1].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode6[1].name).toEqual('my_project1');
        expect(testSortCode6[1].source_dirs[0].name).toEqual('my_folder2');
        expect(testSortCode6[1].source_dirs[0].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode6[1].source_dirs[0].sources[1].filename).toEqual('code1.adb');
        expect(testSortCode6[1].source_dirs[1].name).toEqual('my_folder1');
        expect(testSortCode6[1].source_dirs[1].sources[0].filename).toEqual('code2.adb');
        expect(testSortCode6[1].source_dirs[1].sources[1].filename).toEqual('code1.adb');
    });

});
