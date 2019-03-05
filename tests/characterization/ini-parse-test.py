#!/usr/bin/env python

import configparser

INI_FILE = """
[Simple Values]
key=value
spaces in keys=allowed
spaces in values=allowed as well
spaces around the delimiter = obviously
you can also use : to delimit keys from values

[All Values Are Strings]
values like this: 1000000
or this: 3.14159265359
are they treated as numbers? : no
integers, floats and booleans are held as: strings
can use the API to get converted values directly: true

[Multiline Values]
chorus: I'm a lumberjack, and I'm okay
    I sleep all night and I work all day

[No Values]
empty string value here =

[You can use comments]
# like this
; or this

# By default only in an empty line.
# Inline comments can be harmful because they prevent users
# from using the delimiting characters as parts of values.
# That being said, this can be customized.

    [Sections Can Be Indented]
        can_values_be_as_well = True
        does_that_mean_anything_special = False
        purpose = formatting for readability
        multiline_values = are
            handled just fine as
            long as they are indented
            deeper than the first line
            of a value
        # Did I mention we can indent comments, too?
"""

config = configparser.ConfigParser()

config.read_string(INI_FILE)

parsed = [(s, config.items(s)) for s in config.sections()]
expected = [('Simple Values', [('key', 'value'), ('spaces in keys', 'allowed'), ('spaces in values', 'allowed as well'), ('spaces around the delimiter', 'obviously'), ('you can also use', 'to delimit keys from values')]), ('All Values Are Strings', [('values like this', '1000000'), ('or this', '3.14159265359'), ('are they treated as numbers?', 'no'), ('integers, floats and booleans are held as', 'strings'), ('can use the api to get converted values directly', 'true')]), ('Multiline Values', [('chorus', "I'm a lumberjack, and I'm okay\nI sleep all night and I work all day")]), ('No Values', [('empty string value here', '')]), ('You can use comments', []), ('Sections Can Be Indented', [('can_values_be_as_well', 'True'), ('does_that_mean_anything_special', 'False'), ('purpose', 'formatting for readability'), ('multiline_values', 'are\nhandled just fine as\nlong as they are indented\ndeeper than the first line\nof a value')])]

assert parsed == expected, "Not parsed correctly"
