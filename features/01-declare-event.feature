Feature: Declare event

    Scenario: The consumer declares two news events and fetch them
        Given "cat" is a declared subject kind with ["release"] actions
        When declaring a new "cat" subject as "hera" with data {"name": "hera"}
        Then an event uid $event0_uid should be retrieved
        When declaring a new "cat" event of "hera" with "release" action and {"name": "ares", "gender": "male"} data
        Then an event uid $event1_uid should be retrieved
        When declaring a new "cat" event of "hera" with "release" action and {"name": "eris", "gender": "female"} data
        Then an event uid $event2_uid should be retrieved
        When fetching "cat" events of "hera"
        Then the following events are retrieved:
            | uid         | action  | content                              |
            | $event0_uid | init    | {"name": "hera"}                     |
            | $event1_uid | release | {"name": "ares", "gender": "male"}   |
            | $event2_uid | release | {"name": "eris", "gender": "female"} |
