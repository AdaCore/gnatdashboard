import { GNAThubService } from './gnathub.service';

const inputFullXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'

                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'

                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'

                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '</lock_access_map>'
                     + ' <lock_access_map  proc="search_team.searcherTask_Type_Body2">'

                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '</lock_access_map>'
                      + '</obj_race_info>'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total2" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'

                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'

                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '</lock_access_map>'
                     + ' <lock_access_map  proc="search_team.searcherTask_Type_Body2">'

                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '<lock_access_entry >'

                      + '<lock_set>'
                      + '<lock name="test" />'
                      + '<lock name="test1" />'
                      + '<lock name="test 2" />'
                      + '</lock_set>'

                      + '<srcpos_set acc="read access">'

                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="2" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="3" column="22"/>'

                      + ' </srcpos_set>'

                      + ' </lock_access_entry>'
                      + '</lock_access_map>'
                      + '</obj_race_info>'

                      + '</objs_in_trouble>';

const expectedFullJSON: any = {
    'Traversal.Threaded_Display.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '2',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '3',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '2',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '3',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }
    ],
    'Traversal.Threaded_Display.Total2': [
                {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '2',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '3',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '2',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body2',
            line: '3',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test, test1, test 2'
        }
    ]
};

// Easy small test, to verify when there's no multiple informations
const inputSmallXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      + '</objs_in_trouble>';

const expectedSmallJSON: any = {
    'Traversal.Threaded_Display.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test'
        }
    ]
};

// Small test, to verify when there's duplicate informations
// Should do same as inputSmallXML
const inputDupXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      + '</objs_in_trouble>';

// Easy small test, to verify when there's no multiple informations
const inputRaceXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display2.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      + '</objs_in_trouble>';

const expectedRaceJSON: any = {
    'Traversal.Threaded_Display.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test'
        }
    ],
    'Traversal.Threaded_Display2.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test'
        }
    ],
};

// Easy small test, to verify when there's multiple informations
const inputLockXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> '
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test1" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> '
                      + '</lock_access_map>'
                      + '</obj_race_info>'
                      + '</objs_in_trouble>';

const expectedLockJSON: any = {
    'Traversal.Threaded_Display.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test1'
        }
    ]
};

// Easy small test, to verify when there's no multiple informations
const inputFileXML: any = '<?xml version="1.0"?>'
                      + '<objs_in_trouble  ptn="0">'
                      +  '<obj_race_info obj_name="Traversal.Threaded_Display.Total" '
                      + 'race_err_kind="unprotected access" ref_count="3" '
                      + 'updater="search_team.searcherTask_Type_Body">'
                      + ' <lock_access_map  proc="search_team.searcherTask_Type_Body">'
                      + '<lock_access_entry >'
                      + '<lock_set><lock name="test" /></lock_set>'
                      + '<srcpos_set acc="read access">'
                      + '<srcpos file="traversal-threaded_display.adb-frameset.html" '
                      + 'line="1" column="22"/>'
                      + '<srcpos file="traversal-threaded_display.adb2-frameset.html" '
                      + 'line="1" column="22"/>'
                      + ' </srcpos_set>'
                      + ' </lock_access_entry> </lock_access_map>'
                      + '</obj_race_info>'
                      + '</objs_in_trouble>';

const expectedFileJSON: any = {
    'Traversal.Threaded_Display.Total': [
        {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb',
            lock_name: 'test'
        }, {
            entry_point: 'search_team.searcherTask_Type_Body',
            line: '1',
            access: 'read',
            err_kind: 'unprotected access',
            file: 'traversal-threaded_display.adb2',
            lock_name: 'test'
        }
    ]
};

describe('GNAThub.service_race-condition', () => {
    let gnathubService: any = new GNAThubService();

    let outputSmallJSON: any = gnathubService.raceToJson(inputSmallXML);
    let outputDupJSON: any = gnathubService.raceToJson(inputDupXML);
    it('race-condition_Small_examples', () => {
        expect(outputSmallJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedSmallJSON['Traversal.Threaded_Display.Total']);
        expect(outputDupJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedSmallJSON['Traversal.Threaded_Display.Total']);
    });

    let outputRaceJSON: any = gnathubService.raceToJson(inputRaceXML);
    it('race-condition_Compare_object_race', () => {
        expect(outputRaceJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedRaceJSON['Traversal.Threaded_Display.Total']);
    });

    let outputLockJSON: any = gnathubService.raceToJson(inputLockXML);
    it('race-condition_Compare_lock_name', () => {
        expect(outputLockJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedLockJSON['Traversal.Threaded_Display.Total']);
    });

    let outputFileJSON: any = gnathubService.raceToJson(inputFileXML);
    it('race-condition_Compare_File_name', () => {
        expect(outputFileJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedFileJSON['Traversal.Threaded_Display.Total']);
    });

    let outputFullJSON: any = gnathubService.raceToJson(inputFullXML);
    it('race-condition_Big_example', () => {
        expect(outputFullJSON['Traversal.Threaded_Display.Total'])
            .toEqual(expectedFullJSON['Traversal.Threaded_Display.Total']);
        expect(outputFullJSON['Traversal.Threaded_Display2.Total'])
            .toEqual(expectedFullJSON['Traversal.Threaded_Display2.Total']);
    });

});
