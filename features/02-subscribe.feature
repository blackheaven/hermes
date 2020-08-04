Feature: Subscribe

    Scenario: The consumer declares two subscriptions and check them
        Given "cat" is a declared subject kind with ["release", "pet"] actions
        When declaring a new "cat" subject as "hera" with data {"name": "hera"}
        Then an event uid $event0_uid should be retrieved
        When declaring a new "cat" subject as "hestia" with data {"name": "hestia"}
        Then an event uid $event1_uid should be retrieved
        When subscribing to the "cat" subject "hera" as "rhea"
        When subscribing to the "cat" subject "hestia" as "rhea"
        When declaring a new "cat" event of "hera" with "release" action and {"name": "eris", "gender": "female"} data
        Then an event uid $event2_uid should be retrieved
        When declaring a new "cat" event of "hestia" with "pet" action and {} data
        Then an event uid $event3_uid should be retrieved
        When fetching notifications of "rhea"
        Then the following notifications are retrieved:
            | kind | subject | uid         |
            | cat  | hera    | $event2_uid |
            | cat  | hestia  | $event3_uid |

    Scenario: The consumer declares two subscriptions, removes one, and check them
        Given "cat" is a declared subject kind with ["release", "pet"] actions
        When declaring a new "cat" subject as "hera" with data {"name": "hera"}
        When declaring a new "cat" subject as "hestia" with data {"name": "hestia"}
        Then an event uid $event1_uid should be retrieved
        When subscribing to the "cat" subject "hera" as "rhea"
        When subscribing to the "cat" subject "hestia" as "rhea"
        When declaring a new "cat" event of "hera" with "release" action and {"name": "eris", "gender": "female"} data
        Then an event uid $event2_uid should be retrieved
        When declaring a new "cat" event of "hestia" with "pet" action and {} data
        Then an event uid $event3_uid should be retrieved
        When unsubscribing to the "cat" subject "hera" as "rhea"
        When fetching notifications of "rhea"
        Then the following notifications are retrieved:
            | kind | subject | uid         |
            | cat  | hestia  | $event3_uid |

    Scenario: The consumer declares two subscriptions and poll them
        Given "cat" is a declared subject kind with ["release", "pet"] actions
        When declaring a new "cat" subject as "hera" with data {"name": "hera"}
        Then an event uid $event0_uid should be retrieved
        When declaring a new "cat" subject as "hestia" with data {"name": "hestia"}
        Then an event uid $event1_uid should be retrieved
        When subscribing to the "cat" subject "hera" as "rhea"
        When declaring a new "cat" event of "hera" with "pet" action and {} data
        When polling notifications of "rhea"
        When subscribing to the "cat" subject "hestia" as "rhea"
        When declaring a new "cat" event of "hera" with "release" action and {"name": "eris", "gender": "female"} data
        Then an event uid $event2_uid should be retrieved
        When declaring a new "cat" event of "hestia" with "pet" action and {} data
        Then an event uid $event3_uid should be retrieved
        Then the following notifications have been pushed:
            | kind | subject | uid         |
            | cat  | hera    | $event2_uid |
            | cat  | hestia  | $event3_uid |
