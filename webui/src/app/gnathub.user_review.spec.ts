import { GNAThubService } from './gnathub.service';

const inputXML: any = { _body : '<?xml version="1.0" encoding="utf-8"?>'
                         + '<audit_trail format="6">'
                         + '<message identifier="592">'
                         + '<audit timestamp="2020-01-24 17:05:07" status="False_Positive" '
                         + 'status_category="NOT_A_BUG" approved="TESTEUR 1" from_source="FALSE">'
                         + 'This is a false positive</audit>'
                         + '<audit timestamp="2020-01-24 16:56:49" status="test 1"'
                         + ' status_category="PENDING" approved="TESTEUR 1" from_source="FALSE">'
                         + 'this is a pending</audit>'
                         + '<audit timestamp="2020-01-24 16:55:12" status="Not_A_Bug"'
                         + ' status_category="NOT_A_BUG" approved="TESTEUR 2" from_source="FALSE">'
                         + 'this is a not a bug</audit>'
                         + '</message>'
                         + '<message identifier="593">'
                         + '<audit timestamp="2020-01-28 13:21:54" status="test 6"'
                         + ' status_category="NOT_A_BUG" approved="TESTEUR 2" from_source="FALSE">'
                         + 'this is a custom status</audit>'
                         + '</message>'
                         + '</audit_trail>'};
const expectedJSON: any = {
    592: {
        review_history: [
            {
                author: 'TESTEUR 1',
                status: 'False_Positive',
                status_priority: -1,
                status_kind: 'NOT_A_BUG',
                date: '2020-01-24 17:05:07',
                from_source: 'FALSE',
                message: 'This is a false positive',
                display_name: 'False Positive'
            },
            {
                author: 'TESTEUR 1',
                status: 'test 1',
                status_priority: -1,
                status_kind: 'PENDING',
                date: '2020-01-24 16:56:49',
                from_source: 'FALSE',
                message: 'this is a pending',
                display_name: 'Test 1'
            },
            {
                author: 'TESTEUR 2',
                status: 'Not_A_Bug',
                status_priority: -1,
                status_kind: 'NOT_A_BUG',
                date: '2020-01-24 16:55:12',
                from_source: 'FALSE',
                message: 'this is a not a bug',
                display_name: 'Not A Bug'
            }
        ],
        user_review : {
            author: 'TESTEUR 1',
            status: 'False_Positive',
            status_priority: -1,
            status_kind: 'NOT_A_BUG',
            date: '2020-01-24 17:05:07',
            from_source: 'FALSE',
            message: 'This is a false positive',
            display_name: 'False Positive'
        }
    },
    593: {
        review_history: [
            {
                author: 'TESTEUR 2',
                status: 'test 6',
                status_priority: -1,
                status_kind: 'NOT_A_BUG',
                date: '2020-01-28 13:21:54',
                from_source: 'FALSE',
                message: 'this is a custom status',
                display_name: 'Test 6'
            }
        ],
        user_review : {
            author: 'TESTEUR 2',
            status: 'test 6',
            status_priority: -1,
            status_kind: 'NOT_A_BUG',
            date: '2020-01-28 13:21:54',
            from_source: 'FALSE',
            message: 'this is a custom status',
            display_name: 'Test 6'
        }
    }
};

describe('GNAThub_service', () => {
    let gnathubService: any = new GNAThubService();
    let outputJSON: any = gnathubService.convertToJson(inputXML);

    it('gnathub.service_user-review', () => {
        expect(outputJSON[592]).toEqual(expectedJSON[592]);
        expect(outputJSON[593]).toEqual(expectedJSON[593]);
    });

});
