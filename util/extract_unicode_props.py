import sys
import re

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
        print("const char *{} = \"\"\\\n    \"".format(self._symbol), end='', file=fp)
        line = 8  # 4 leading spaces, open quote, close quote, at least 2 trailing spaces

        for codepoint in self._codepoints:
            # Codepoints are 6/7 chars: \u#####
            size = 6 if codepoint <= 0xFFFF else 7
            if line + size > MAX_LINE_LEN:
                print("\"\\\n    \"", end='', file=fp)
                line = 8
            print("\\u{0:0{1}X}".format(codepoint, 4), end='', file=fp)
            line += size

        for coderange in self._ranges:
            # Ranges are 13 chars: \u####-\u####
            size = 6 if coderange.start() <= 0xFFFF else 7
            size += 6 if coderange.end() <= 0xFFFF else 7
            size += 1
            if line + size > MAX_LINE_LEN:
                print("\"\\\n    \"", end='', file=fp)
                line = 8
            print("\\u{0:0{1}X}-\\u{2:0{3}X}".format(coderange.start(), 4, coderange.end(), 4), end='', file=fp)
            line += size

        print("\";\n", file=fp)


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
        eprint("ERROR: Unable to read property file {}: {}".format(filename, str(ex)))
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
        eprint("ERROR: Unable to read property file {}: {}".format(filename, str(ex)))
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


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print("Usage: python extract_unicode_props.py <DerivedCoreProperties.txt> <UnicodeData.txt> [output-file]")
        exit(1)

    propsfile = sys.argv[1]
    unicodefile = sys.argv[2]
    output = sys.argv[3] if len(sys.argv) > 3 else None

    if not parse_props_file(propsfile, unicodefile, output):
        exit(2)
    exit(0)
