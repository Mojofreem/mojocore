import sys
import re
import argparse
import os.path
from itertools import groupby
from functools import reduce


UAX_DERIVED_GENERAL_CATEGORY = 'DerivedGeneralCategory.txt'
UAX_DERIVED_CORE_PROPERTIES = 'DerivedCoreProperties.txt'
UAX_SCRIPTS = 'Scripts.txt'
UAX_UNICODE_DATA = 'UnicodeData.txt'

INCLUDE_DEPRECATED = False
MAX_LINE_LEN = 78

RE_PROPERTY_LINE = re.compile('^#\s+Derived Property:\s+[^(]+(?P<deprecated>\(deprecated\))\s+$', re.DOTALL)
RE_GLYPH_LINE = re.compile('^(?P<codepoint>[0-9a-fA-F]{4,5})(?P<range>..(?P<rangepoint>[0-9a-fA-F]{4,5}))?[^#]+#\s+(?P<class>[^ \t]+)', re.DOTALL)


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


class CodeRange(object):
    def __init__(self, start, end):
        self._start = start
        self._end = end

    def start(self):
        return self._start

    def end(self):
        return self._end

    def add_to_range(self, codepoint):
        if codepoint >= self._start and codepoint <= self._end:
            return True
        if codepoint == self._start - 1:
            self._start = codepoint
            return True
        if codepoint == self._end + 1:
            self._end = codepoint
            return True
        return False

    def merge_ranges(self, mrange):
        if mrange == self:
            return False
        if mrange.start() < self._start and mrange.end() >= self._start - 1:
            self._start = mrange.start()
            if mrange.end() > self._end:
                self._end = mrange.end()
            return True
        if mrange.start() <= self._end + 1 and mrange.end() >= self._start - 1:
            if mrange.start() < self._start:
                self._start = mrange.start()
            if mrange.end() > self._end:
                self._end = mrange.end()
            return True
        return False


class PropertySet(object):
    def __init__(self, group, symbol):
        self._group = group
        self._symbol = symbol
        self._codepoints = []
        self._ranges = []

    def get_codepoints(self):
        return self._codepoints

    def get_ranges(self):
        return self._ranges

    def unicode_class(self):
        if len(self._group) == 1:
            return '{}*'.format(self._group)
        return self._group

    def is_group(self, group):
        return group[:len(self._group)] == self._group

    def merge_codepoint(self, codepoint):
        for coderange in self._ranges:
            if coderange.add_to_range(codepoint):
                return True
        if (codepoint + 1) in self._codepoints:
            self.add_range(codepoint, codepoint + 1)
            return True
        if (codepoint - 1) in self._codepoints:
            self.add_range(codepoint - 1, codepoint)
            return True
        return False

    def add_codepoint(self, codepoint):
        if self.merge_codepoint(codepoint):
            return
        self._codepoints.append(codepoint)

    def merge_range(self, newrange):
        for coderange in self._ranges:
            if coderange.merge_ranges(newrange):
                return True
        return False

    def add_range(self, code1, code2):
        newrange = CodeRange(code1, code2)
        if self.merge_range(newrange):
            return
        self._ranges.append(newrange)

    def compact(self):
        merged = True
        while merged:
            merged = False
            for codepoint in self._codepoints:
                if self.merge_codepoint(codepoint):
                    self._codepoints.remove(codepoint)
                    merged = True
            for checkrange in self._ranges:
                if self.merge_range(checkrange):
                    self._ranges.remove(checkrange)
                    merged = True

    def generate_class_def(self, fp):
        def escape_char(codepoint):
            return 'u' if codepoint <= 0xFFFF else 'U'

        def padding(codepoint):
            return 4 if codepoint <= 0xFFFF else 6

        print("const char {}[] =\n    \"".format(self._symbol), end='', file=fp)
        line = 8  # 4 leading spaces, open quote, close quote, at least 2 trailing spaces

        for codepoint in self._codepoints:
            # Codepoints are 7/9 chars: \\u#### | \\U######
            size = 7 if codepoint <= 0xFFFF else 9
            if line + size > MAX_LINE_LEN:
                print("\"\\\n    \"", end='', file=fp)
                line = 8
            print("\\\\{2}{0:0{1}X}".format(codepoint, padding(codepoint),
                                          escape_char(codepoint)), end='', file=fp)
            line += size

        for coderange in self._ranges:
            # Ranges are 15-19 chars: \\u####-\\u#### | \\U######-\\U######
            size = 7 if coderange.start() <= 0xFFFF else 9
            size += 7 if coderange.end() <= 0xFFFF else 9
            size += 1
            if line + size > MAX_LINE_LEN:
                print("\"\\\n    \"", end='', file=fp)
                line = 8
            print("\\\\{4}{0:0{1}X}-\\\\{5}{2:0{3}X}".format(coderange.start(), padding(coderange.start()),
                                                         coderange.end(), padding(coderange.end()),
                                                         escape_char(coderange.start()),
                                                         escape_char(coderange.end())), end='', file=fp)
            line += size

        print("\";\n", file=fp)


UAX_GENERAL_CATEGORY_GROUPS = {'LC': ['Lu', 'Ll', 'Lt'],
                               'L': ['Lu', 'Ll', 'Lt', 'Lm', 'Lo'],
                               'M': ['Mn', 'Mc', 'Me'],
                               'N': ['Nd', 'Nl', 'No'],
                               'P': ['Pc', 'Pd', 'Ps', 'Pe', 'Pi', 'Pf', 'Po'],
                               'S': ['Sm', 'Sc', 'Sk', 'So'],
                               'Z': ['Zs', 'Zl', 'Zp'],
                               'C': ['Cc', 'Cf', 'Cs', 'Co', 'Cn']}

UAX_GENERAL_CATEGORY_MAP = {'Lu': 'Uppercase_Letter',
                            'Ll': 'Lowercase_Letter',
                            'Lt': 'Titlecase_Letter',
                            'LC': 'Cased_Letter',
                            'Lm': 'Modifier_Letter',
                            'Lo': 'Other_Letter',
                            'L': 'Letter',
                            'Mn': 'Nonspacing_Mark',
                            'Mc': 'Spacing_Mark',
                            'Me': 'Enclosing_Mark',
                            'M': 'Mark',
                            'Nd': 'Decimal_Number',
                            'Nl': 'Letter_Number',
                            'No': 'Other_Number',
                            'N': 'Number',
                            'Pc': 'Connector_Punctuation',
                            'Pd': 'Dash_Punctuation',
                            'Ps': 'Open_Punctuation',
                            'Pe': 'Close_Punctuation',
                            'Pi': 'Initial_Punctuation',
                            'Pf': 'Final_Punctuation',
                            'Po': 'Other_Punctuation',
                            'P': 'Punctuation',
                            'Sm': 'Math_Symbol',
                            'Sc': 'Currency_Symbol',
                            'Sk': 'Modifier_Symbol',
                            'So': 'Other_Symbol',
                            'S': 'Symbol',
                            'Zs': 'Space_Separator',
                            'Zl': 'Line_Separator',
                            'Zp': 'Paragraph_Separator',
                            'Z': 'Separator',
                            'Cc': 'Control',
                            'Cf': 'Format',
                            'Cs': 'Surrogate',
                            'Co': 'Private_Use',
                            'Cn': 'Unassigned',
                            'C': 'Other'}


PROPERTY_SET_MAP = {'M': 'unicode_combining_marks',
                    'N': 'unicode_numeric',
                    'P': 'unicode_punctuation',
                    'Z': 'unicode_whitespace',
                    'L': 'unicode_letter',
                    'Lu': 'unicode_uppercase',
                    'Ll': 'unicode_lowercase'}


def parse_props_file(propsfile, unicodefile, output):
    properties = []
    deprecated = False

    for group, symbol in PROPERTY_SET_MAP.items():
        properties.append(PropertySet(group, symbol))

    eprint("Parsing properties file \"{}\"...".format(propsfile))

    try:
        with open(propsfile, 'rt') as fp:
            for line in fp:
                m = RE_PROPERTY_LINE.match(line)
                if m is not None:
                    deprecated = True if m.group('deprecated') is not None else False
                elif not deprecated or INCLUDE_DEPRECATED:
                    m = RE_GLYPH_LINE.match(line)
                    if m is not None:
                        codepoint = m.group('codepoint')
                        rangepoint = m.group('rangepoint')
                        unicode_class = m.group('class')
                        for group in properties:
                            if group.is_group(unicode_class):
                                if rangepoint is not None:
                                    group.add_range(int(codepoint, 16), int(rangepoint, 16))
                                else:
                                    group.add_codepoint(int(codepoint, 16))
    except Exception as ex:
        eprint("ERROR: Unable to read property file {}: {}".format(propsfile, str(ex)))
        return False

    eprint("Parsing unicode data file \"{}\"...".format(unicodefile))

    try:
        with open(unicodefile, 'rt') as fp:
            for line in fp:
                parts = line.split(';')
                if len(parts) == 15:
                    codepoint = int(parts[0], 16)
                    unicode_class = parts[2]
                    for group in properties:
                        if group.is_group(unicode_class):
                            group.add_codepoint(codepoint)
    except Exception as ex:
        eprint("ERROR: Unable to read property file {}: {}".format(unicodefile, str(ex)))
        return False

    eprint("Compacting property set codepoint ranges...")

    for group in properties:
        eprint("    Compacting property set {}...".format(group.unicode_class()))
        group.compact()

    fp = sys.stdout
    if output is not None:
        eprint("Writing property set source file...")
        try:
            fp = open(output, 'wt')
            print("// This file was generated from the unicode database by the extract_unicode_props.py script\n",
                  file=fp)
        except Exception as ex:
            eprint("ERROR: Failed to open output file {}: {}".format(output, str(ex)))

    for group in properties:
        group.generate_class_def(fp)

    eprint("Done!")

    return True


class CodepointSet(object):
    def __init__(self):
        self._refined = False
        self._codepoints = []
        self._ranges = []

    def add_codepoint(self, codepoint):
        codepoint = int(codepoint, 16)
        if codepoint in self._codepoints:
            return
        self._refined = False
        self._codepoints.append(codepoint)

    def add_range(self, start, end):
        pair = (int(start, 16), int(end, 16))
        if pair in self._ranges:
            return
        self._refined = False
        self._ranges.append(pair)

    def _refine(self):
        def ranges(i):
            for a, b in groupby(enumerate(i), lambda pair: pair[1] - pair[0]):
                b = list(b)
                yield b[0][1], b[-1][1]

        def merge(listing, item):
            if listing:
                if listing[-1][1] + 1 >= item[0]:
                    listing[-1] = listing[-1][0], max(listing[-1][1], item[1])
                    return listing
            listing.append(item)
            return listing

        if self._refined:
            return
        self._refined = True

        # Merge singlet codepoints into ranges (including ranges of len 1)
        merged = list(ranges(self._codepoints))

        # sort and reduce ranges
        grouped = reduce(merge, sorted(self._ranges + merged, key=lambda tup: tup[0]), [])

        # extract single value ranges
        singlets = list(filter(lambda item: item[0] == item[1], grouped))

        # retain refined ranges
        self._ranges = sorted(list(set(grouped) - set(singlets)), key=lambda tup: tup[0])

        # store single codepoints
        self._codepoints = sorted([x[0] for x in singlets])

    def get_codepoints(self):
        self._refine()
        return self._codepoints

    def get_ranges(self):
        self._refine()
        return self._ranges

    def generate_source_string(self, fp, linelen, lead):
        def codepoint_details(codepoint):
            if codepoint <= 0xFFFF:
                return 7, 4, '', ''  # \\u####
            if codepoint <= 0xFFFFF:
                return 10, 5, '{', '}'  # \\u{#####}
            return 11, 6, '{', '}'  # \\u{######}

        self._refine()

        linelen -= 3 # Account for leading and trailing quotes, and semicolon
        line = lead - 3

        print("\"", end='', file=fp)

        for codepoint in self._codepoints:
            n, pad, pre, post = codepoint_details(codepoint)
            if n + line >= linelen:
                print("\"\n    \"", end='', file=fp)
                line = 5
            line += n
            print('\\\\u{0}{1:0{2}X}{3}'.format(pre, codepoint, pad, post), end='', file=fp)

        for code_range in self._ranges:
            n, pad, pre, post = codepoint_details(code_range[0])
            if n + 1 + line >= linelen:
                print("\"\n    \"", end='', file=fp)
                line = 5
            line += n + 1
            print('\\\\u{0}{1:0{2}X}{3}-'.format(pre, code_range[0], pad, post), end='', file=fp)

            n, pad, pre, post = codepoint_details(code_range[1])
            if n + line >= linelen:
                print("\"\n    \"", end='', file=fp)
                line = 5
            line += n
            print('\\\\u{0}{1:0{2}X}{3}'.format(pre, code_range[1], pad, post), end='', file=fp)

        print("\";", file=fp)

    def __add__(self, other):
        if type(other) != type(self):
            raise Exception("Undefined action adding {} to {}".format(type(other), type(self)))
        result = CodepointSet()
        result._codepoints = other.get_codepoints() + self._codepoints
        result._ranges = other.get_ranges() + self._ranges
        return result


class UaxCategories(object):
    def __init__(self, categories_file):
        self._categories = {}
        self._valid = False
        self._parse(categories_file)

    def valid(self):
        return self._valid

    def _parse(self, categories_file):
        self._valid = True
        try:
            with open(categories_file, 'rt') as fp:
                for line in fp:
                    if len(line.strip()) == 0 or line.strip()[0] == '#':
                        continue
                    parts = line.split(';')
                    if len(parts) >= 2:
                        codepoints = parts[0].split('..')
                        category = parts[1].strip()
                        if len(category) == 0:
                            continue
                        category = category.split('#')[0].strip()
                        if category not in self._categories.keys():
                            self._categories[category] = CodepointSet()
                        if len(codepoints) == 1:
                            self._categories[category].add_codepoint(codepoints[0])
                        else:
                            self._categories[category].add_range(codepoints[0], codepoints[1])
        except Exception as ex:
            eprint("ERROR: Unable to read UAX general categories file [{}]: {}".format(categories_file, str(ex)))
            self._valid = False
            return False

    def get_categories(self):
        return self._categories.keys()

    def get_category_tuple(self, category):
        if category in UAX_GENERAL_CATEGORY_MAP.keys():
            return category, UAX_GENERAL_CATEGORY_MAP[category]
        for abbr, name in UAX_GENERAL_CATEGORY_MAP.items():
            if name == category:
                return abbr, name

    def get_category_codepoint_set(self, category):
        if category in UAX_GENERAL_CATEGORY_GROUPS.keys():
            compound = None
            for item in UAX_GENERAL_CATEGORY_GROUPS[category]:
                if item not in self._categories.keys():
                    eprint("ERROR: General category [{}] was not found".format(item))
                    return None
                if compound is None:
                    compound = self._categories[item]
                else:
                    compound = compound + self._categories[item]
            return compound

        if category in self._categories.keys():
            return self._categories[category]

        eprint("ERROR: General category [{}] was not found".format(category))
        return None


class UaxScripts(object):
    def __init__(self, script_file):
        self._scripts = {}
        self._valid = False
        self._parse(script_file)

    def valid(self):
        return self._valid

    def _parse(self, script_file):
        self._valid = True
        try:
            with open(script_file, 'rt') as fp:
                for line in fp:
                    if len(line.strip()) == 0 or line.strip()[0] == '#':
                        continue
                    parts = line.split(';')
                    if len(parts) >= 2:
                        codepoints = parts[0].split('..')
                        script = parts[1].strip()
                        if len(script) == 0:
                            continue
                        script = script.split('#')[0].strip()
                        if script not in self._scripts.keys():
                            self._scripts[script] = CodepointSet()
                        if len(codepoints) == 1:
                            self._scripts[script].add_codepoint(codepoints[0])
                        else:
                            self._scripts[script].add_range(codepoints[0], codepoints[1])
        except Exception as ex:
            eprint("ERROR: Unable to read UAX scripts file [{}]: {}".format(script_file, str(ex)))
            self._valid = False
            return False

    def get_script_names(self):
        return self._scripts.keys()

    def get_script_codepoint_set(self, name):
        if name not in self._scripts.keys():
            return None
        return self._scripts[name]


def parse_uax_derived_general_category(path):
    if not os.path.exists(path):
        eprint("ERROR: The UAX derived general category file [{}] was not found".format(path))
        return None
    print("Parsing the UAX general categories file...")
    categories = UaxCategories(path)
    if not categories.valid():
        return None
    return categories


def parse_uax_scripts(path):
    if not os.path.exists(path):
        eprint("ERROR: The UAX scripts file [{}] was not found".format(path))
        return None
    print("Parsing the UAX scripts file...")
    scripts = UaxScripts(path)
    if not scripts.valid():
        return None
    return scripts


def list_details(categories, scripts):
    names = sorted(scripts.get_script_names())
    count = len(names)
    print("Identified {} unique scripts:".format(count))
    print("=================================\n")

    if count > 0:
        lens = [len(name) for name in names]
        longest = sorted(lens)[-1]
        cols = int(MAX_LINE_LEN / (longest + 2))
        num = int(len(names) / cols)
        if len(names) % cols:
            num += 1
        table = []
        for col in range(0, cols):
            table.append(names[col * num: (col + 1) * num])

        col = 0
        line = 0
        for pos in range(0, len(names)):
            print('  {0:{1}}'.format(table[col][line], longest), end='')
            col += 1
            if col >= cols:
                col = 0
                line += 1
                print()

    print("\n\n{} general categories ({} compound parent categories):".format(len(UAX_GENERAL_CATEGORY_MAP.keys()),
                                                                          len(UAX_GENERAL_CATEGORY_GROUPS.keys())))
    print("======================================================\n")
    print('    Set    Description             Compound group')
    print('    ----  ----------------------  -----------------------')
    for key, value in UAX_GENERAL_CATEGORY_MAP.items():
        print('     {:2}   {:22}  '.format(key, value), end='')
        if key in UAX_GENERAL_CATEGORY_GROUPS.keys():
            print('+'.join(UAX_GENERAL_CATEGORY_GROUPS[key]))
        else:
            print()


def generate_source(categories, scripts, properties, script_names, header, source):
    if header is None and source is None:
        return True

    groups = []

    print("Gathering data to generate source tables:")
    for property in properties:
        data = categories.get_category_codepoint_set(property)
        if data is None:
            return False
        detail = categories.get_category_tuple(property)
        groups.append((detail[0], detail[1], data))
        print("    Adding property [{}:{}]...".format(detail[0], detail[1]))

    for script in script_names:
        data = scripts.get_script_codepoint_set(script)
        if data is None:
            eprint("ERROR: Unknown script [{}]".format(script))
            return False
        groups.append((None, script, data))
        print("    Adding script [{}]...".format(script))

    if header is not None:
        print("Generating header file [{}]...".format(header))
        try:
            with open(header, 'wt') as fp:
                print('''// This file was automatically generated from the UAX unicode database
// via the extract_unicode_props.py script

#ifndef _MOJOCORE_UAX_DB_DEFINITIONS_
#define _MOJOCORE_UAX_DB_DEFINITIONS_

#ifndef mojo_unicode_class_s
struct mojo_unicode_class_s {
    const char *abbreviation;
    const char *property;
    const char *class_string;
}
#endif // mojo_unicode_class_s        

#ifndef mojo_unicode_class_t
typedef struct mojo_unicode_class_s mojo_unicode_class_t;
#endif // mojo_unicode_class_t
''', file=fp)
                for group in groups:
                    print('extern const char _uax_db_{}[];'.format(group[1]), file=fp)

                print('\nmojo_unicode_class_t uax_db_import_table[] = {', file=fp)

                for group in groups:
                    print('    {{{}, {}, _uax_db_{}}},'.format(group[0] if group[0] is not None else 'NULL',
                                                               group[1], group[1]), file=fp)

                print('    {NULL, NULL, NULL}', file=fp)

                print('};\n\n#endif // _MOJOCORE_UAX_DB_DEFINITIONS_', file=fp)
        except Exception as ex:
            eprint("ERROR: Unable to write header file [{}] output: {}".format(header, str(ex)))
            return False

    if source is not None:
        print("Generating source file [{}]...".format(source))
        try:
            with open(source, 'wt') as fp:
                for group in groups:
                    lead = 'const char _uax_db_{}[] = '.format(group[1])
                    print(lead, end='', file=fp)
                    group[2].generate_source_string(fp, MAX_LINE_LEN, len(lead))
                    print('', file=fp)
        except Exception as ex:
            eprint("ERROR: Unable to write source file [{}] output: {}".format(source, str(ex)))
            return False

    print("Generated {} requested entries".format(len(groups)))
    return True


def parser_script():
    parser = argparse.ArgumentParser(description='Utility for parsing the Unicode Standard Annex (UAX)')
    parser.add_argument('-i', '--header', type=str, action='store', default=None,
                        dest='header', metavar='FILE',
                        help='C style header file to generate for the parsed data')
    parser.add_argument('-s', '--source', type=str, action='store', default=None,
                        dest='source', metavar='FILE',
                        help='C style source file to generate for the parsed data')
    parser.add_argument('-p', '--property', type=str, action='append',
                        metavar='PROP', dest='properties', default=[],
                        help='Unicode property class to parse')
    parser.add_argument('-c', '--script', type=str, action='append',
                        metavar='SCRIPT', dest='scripts', default=[],
                        help='Unicode script glyphs to parse')
    parser.add_argument('-u', '--uax', type=str, action='store', default='',
                        dest='uax', metavar='PATH',
                        help='path to the UAX database files directory')
    parser.add_argument('-l', '--list', action='store_true', default=False,
                        dest='listing',
                        help='List the known script and property values from the UAX')

    args = parser.parse_args()

    # Identify the UAX file locations
    category_file = os.path.join(args.uax, UAX_DERIVED_GENERAL_CATEGORY)
    if not os.path.exists(category_file):
        category_file = os.path.join(args.uax, 'extracted', UAX_DERIVED_GENERAL_CATEGORY)

    script_file = os.path.join(args.uax, UAX_SCRIPTS)

    categories = None
    scripts = None

    if len(args.properties) > 0 or args.listing:
        categories = parse_uax_derived_general_category(category_file)
        if categories is None:
            return False

    if len(args.scripts) > 0 or args.listing:
        scripts = parse_uax_scripts(script_file)
        if scripts is None:
            return False

    if args.listing:
        list_details(categories, scripts)

    if not generate_source(categories, scripts, args.properties, args.scripts, args.header, args.source):
        return False

    return True


if __name__ == '__main__':
    if not parser_script():
        exit(1)
    exit(0)
