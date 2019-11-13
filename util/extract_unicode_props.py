import sys
import re

INCLUDE_DEPRECATED = False
MAX_LINE_LEN = 78

RE_PROPERTY_LINE = re.compile('^#\s+Derived Property:\s+[^(]+(?P<deprecated>\(deprecated\))\s+$', re.DOTALL)
RE_GLYPH_LINE = re.compile('^(?P<codepoint>[0-9a-fA-F]{4,5})(?P<range>..(?P<rangepoint>[0-9a-fA-F]{4,5}))?[^#]+#\s+(?P<class>[^ \t]+)', re.DOTALL)


'''
int parseUtf8EncodingByteLen(int c) {
    if(c > 65535) {
        return 4;
    } else if(c >2047) {
        return 3;
    } else if(c > 127) {
        return 2;
    }
    return 1;
}

unsigned int parseUtf8EncodeCodepoint(int c) {
    if(c <= 127) { // 1 byte
        return (unsigned int)c;
    } else if(c <= 2047) { // 2 byte (5 prefix bits)
        return 0xC080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0x7C0u) << 2u);
    } else if(c <= 65535) { // 3 byte (4 prefix bits)
        return 0xE08080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0xFC0u) << 2u) | (((unsigned int)c & 0xF000u) << 4u);
    } else { // 4 byte (3 prefix bits)
        return 0xF08080u | ((unsigned int)c & 0x3Fu) | (((unsigned int)c & 0xFC0u) << 2u) | (((unsigned int)c & 0x3F000u) << 4u) | ((unsigned int)c & 0x7000000u);
    }
}
'''


def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


# Returns the number of bytes needed to encode the codepoint
def utf8_enc_byte_count(codepoint):
    if codepoint > 65535:
        return 4
    elif codepoint > 2047:
        return 3
    elif codepoint > 127:
        return 2
    return 1


def utf8_byte_enc_range(count):
    if count == 1:
        return 0, 127
    elif count == 2:
        return 128, 2047
    elif count == 3:
        return 2048, 65535
    else:
        return 65535, 0x10FFFF


# Returns a tuple, containing the number of bytes to encode a codepoint, followed
# by the actual bytes for the encoding of the codepoint
def utf8_enc_bytes(codepoint):
    count = utf8_enc_byte_count(codepoint)
    if count == 4:
        return 4, ((codepoint & 0x7000000) >> 18) | 0xF0, (codepoint & 0x3F000) >> 12, (codepoint & 0xFC0) >> 6, codepoint & 0x3F
    elif count == 3:
        return 3, ((codepoint & 0xF000) >> 12) | 0xE0, (codepoint & 0xFC0) >> 6, codepoint & 0x3F
    elif count == 2:
        return 2, ((codepoint & 0x7C0) >> 6) | 0xC0, codepoint & 0x3F
    else:
        return 1, codepoint


UTF8_ENC_LOW_BYTE_FULL_RANGE = 0x3F         # 10xxxxxx
UTF8_ENC_ONE_BYTE_FULL_RANGE = 0x7F         # 0xxxxxxx
UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE = 0xDF    # 110xxxxx
UTF8_ENC_HIGH_THREE_BYTE_FULL_RANGE = 0xEF  # 1110xxxx
UTF8_ENC_HIGH_FOUR_BYTE_FULL_RANGE = 0xF7   # 11110xxx

UTF8_ENC_HIGH_TWO_BYTE_BASE_VALUE = 0xD0    # 110xxxxx
UTF8_ENC_HIGH_THREE_BYTE_BASE_VALUE = 0xE0  # 1110xxxx
UTF8_ENC_HIGH_FOUR_BYTE_BASE_VALUE = 0xF0   # 11110xxx


def utf8_enc_split_range_by_bytes(code_a, code_b):
    c_a, *b_a = utf8_enc_bytes(code_a)
    c_b, *b_b = utf8_enc_bytes(code_b)

    result = []
    if c_a == c_b:
        # The byte count in the range is equal
        return utf8_enc_generate_ranges_by_bytes(c_a, b_a, b_b)

    # The range crosses a byte encoding boundary
    if c_a == 1:
        result += utf8_enc_generate_ranges_by_bytes(1, b_a, [UTF8_ENC_ONE_BYTE_FULL_RANGE])
    elif c_a == 2:
        result += utf8_enc_generate_ranges_by_bytes(2, b_a, [UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE,
                                                             UTF8_ENC_LOW_BYTE_FULL_RANGE])
    elif c_a == 3:
        result += utf8_enc_generate_ranges_by_bytes(3, b_a, [UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE,
                                                             UTF8_ENC_LOW_BYTE_FULL_RANGE,
                                                             UTF8_ENC_LOW_BYTE_FULL_RANGE])

    for count in range(c_a + 1, c_b):
        if count == 2:
            result += utf8_enc_generate_ranges_by_bytes(2, [0, 0],
                                                        [UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE,
                                                         UTF8_ENC_LOW_BYTE_FULL_RANGE])
        elif count == 3:
            result += utf8_enc_generate_ranges_by_bytes(3, [0, 0, 0],
                                                        [UTF8_ENC_HIGH_TWO_BYTE_FULL_RANGE,
                                                         UTF8_ENC_LOW_BYTE_FULL_RANGE,
                                                         UTF8_ENC_LOW_BYTE_FULL_RANGE])

    if c_b == 2:
        result += utf8_enc_generate_ranges_by_bytes(2, [b_b[0], 0], b_b)
    elif c_b == 3:
        result += utf8_enc_generate_ranges_by_bytes(3, [b_b[0], 0, 0], b_b)
    elif c_b == 4:
        result += utf8_enc_generate_ranges_by_bytes(4, [b_b[0], 0, 0, 0], b_b)

    return result


def utf8_enc_generate_ranges_by_bytes(count, b_a, b_b):
    # Byte order for documenting byte encodings: DCBA
    # a_ references the start, b_ references the end
    # i___ references an iterated intermediary value between two points:
    #     _x__ - ABCD, the byte identifier
    #     __x_ - ab0, range base, a, b, or zero
    #     ___x - abHL, range end, a (exclusive), b (exclusive), high byte max value, low byte max value
    # H references the max value for the high byte
    # L references the max value for the low bytes

    result = []

    if count == 1:
        # aA - bA
        result = [[count, b_a, b_b]]

    elif count == 2:
        if b_a[0] == b_b[0]:
            # high bytes are equal
            # aA - bA
            result = [[count, b_a, b_b]]
        else:
            # aB, aA - aL
            # iBab, 0 - L
            # bB, 0 - bA
            result.append([count, b_a, [b_a[0], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for high in range(b_a[0] + 1, b_b[0]):
                result.append([count, [high, 0], [high, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            result.append([count, [b_b[0], 0], b_b])

    elif count == 3:
        if b_a[0] == b_b[0]:
            # high byte is equal
            if b_a[1] == b_b[1]:
                # mid byte is equal
                # aC, aB, aA - bA
                result = [[count, b_a, b_b]]
            else:
                # aC, aB, aA - L
                # aC, iBab, 0 - L
                # aC, bB, 0 - bA
                result.append([count, b_a, [b_a[0], b_a[1], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                for mid in range(b_a[1] + 1, b_b[1]):
                    result.append([count, [b_a[0], mid, 0], [b_a[0], mid, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                result.append([count, [b_b[0], b_b[1], 0], b_b])
        else:
            # aC, aB, aA - L
            # aC, iBaL, 0 - L
            # iCab, iB0L, 0 - L
            # bC, iB0b, 0 - L
            # bC, bB, 0 - bA
            result.append([count, b_a, [b_a[0], b_a[1], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for mid in range(b_a[1] + 1, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                result.append([count, [b_a[0], mid, 0], [b_a[0], mid, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for high in range(b_a[0] + 1, b_b[0]):
                for mid in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                    result.append([count, [high, mid, 0], [high, mid, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for mid in range(0, b_b[1]):
                result.append([count, [b_b[0], mid, 0], [b_b[0], mid, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            result.append([count, [b_b[0], b_b[1], 0], b_b])

    elif count == 4:
        if b_a[0] == b_b[0]:
            # high byte is equal
            if b_a[1] == b_b[1]:
                # midhigh byte is equal
                if b_a[2] == b_b[2]:
                    # midlow byte is equal
                    # aD, aC, aB, aA - bA
                    result = [[count, b_a, b_b]]
                else:
                    # aD, aC, aB, aA - L
                    # aD, aC, iBab, 0 - L
                    # aD, aC, bB, 0 - bA
                    result.append([count, b_a, [b_a[0], b_a[1], b_a[2], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                    for midlow in range(b_a[2], b_b[2]):
                        result.append([count, [b_a[0], b_a[1], midlow, 0],
                                       [b_a[0], b_a[1], midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                    result.append([count, [b_b[0], b_b[1], b_b[2], 0], b_b])
            else:
                # aD, aC, aB, aA - L
                # aD, aC, iBaL, 0 - L
                # aD, iCab, iB0L, 0 - L
                # aD, bC, iB0b, 0 - L
                # aD, bC, bB, 0 - bA
                result.append([count, b_a, [b_a[0], b_a[1], b_a[2], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                for midlow in range(b_a[2], UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                    result.append([count, [b_a[0], b_a[1], midlow, 0],
                                   [b_a[0], b_a[1], midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                for midhigh in range(b_a[1] + 1, b_b[1]):
                    for midlow in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                        result.append([count, [b_a[0], midhigh, midlow, 0],
                                       [b_a[0], midhigh, midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                for midlow in range(0, b_b[2]):
                    result.append([count, [b_b[0], b_b[1], midlow, 0],
                                   [b_b[0], b_b[1], midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

                result.append([count, [b_b[0], b_b[1], b_b[2], 0], b_b])
        else:
            # aD, aC, aB, aA - L
            # aD, aC, iBaL, 0 - L
            # aD, iCab, iB0L, 0 - L
            # iDab, iC0L, iB0L, 0 - L
            # bD, iC0b, iB0L, 0 - L
            # bD, bC, iB0b, 0 - L
            # bD, bC, bB, 0 - bA
            result.append([count, b_a, [b_a[0], b_a[1], b_a[2], UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for midlow in range(b_a[2], UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                result.append([count, [b_a[0], b_a[1], midlow, 0],
                               [b_a[0], b_a[1], midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for midhigh in range(b_a[1] + 1, b_b[1]):
                for midlow in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                    result.append([count, [b_a[0], midhigh, midlow, 0],
                                   [b_a[0], midhigh, midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for high in range(b_a[0], b_b[0]):
                for midhigh in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                    for midlow in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                        result.append([count, [high, midhigh, midlow, 0],
                                       [high, midhigh, midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for midhigh in range(b_a[1] + 1, b_b[1]):
                for midlow in range(0, UTF8_ENC_LOW_BYTE_FULL_RANGE + 1):
                    result.append([count, [b_a[0], midhigh, midlow, 0],
                                   [b_a[0], midhigh, midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            for midlow in range(0, b_b[2]):
                result.append([count, [b_b[0], b_b[1], midlow, 0],
                               [b_b[0], b_b[1], midlow, UTF8_ENC_LOW_BYTE_FULL_RANGE]])

            result.append([count, [b_b[0], b_b[1], b_b[2], 0], b_b])

    return result


class UnicodeByteTree(object):
    def __init__(self, prop_set):
        self._prop_set = prop_set
        self._single_byte = []
        self._two_byte = {}
        self._three_byte = {}
        self._four_byte = {}
        self._build_tree()

    def _build_tree(self):
        for codepoint in self._prop_set.get_codepoints():
            count, *bytes = utf8_enc_bytes(codepoint)
            self._enc_prefix(count, bytes)
            if count == 1:
                if codepoint not in self._single_byte:
                    self._single_byte.append(codepoint)
            elif count == 2:
                if bytes[1] not in self._two_byte[bytes[0]]:
                    self._two_byte[bytes[0]].append(bytes[1])
            elif count == 3:
                if bytes[2] not in self._three_byte[bytes[0]][bytes[1]]:
                    self._three_byte[bytes[0]][bytes[1]].append(bytes[2])
            elif count == 4:
                if bytes[3] not in self._four_byte[bytes[0]][bytes[1]][bytes[2]]:
                    self._four_byte[bytes[0]][bytes[1]][bytes[2]].append(bytes[3])

        for crange in self._prop_set.get_ranges():
            subranges = utf8_enc_split_range_by_bytes(crange.start(), crange.end())
            for entry in subranges:
                print(entry)
                count, start, end = entry
                self._enc_prefix(count, start)
                if count == 1:
                    for val in range(start[0], end[0] + 1):
                        if val not in self._single_byte:
                            self._single_byte.append(val)
                elif count == 2:
                    arr = self._two_byte[start[0]]
                    for val in range(start[1], end[0] + 1):
                        if val not in arr:
                            arr.append(val)
                elif count == 3:
                    arr = self._three_byte[start[0]][start[1]]
                    for val in range(start[2], end[2] + 1):
                        if val not in arr:
                            arr.append(val)
                elif count == 4:
                    arr = self._four_byte[start[0]][start[1]][start[2]]
                    for val in range(start[3], end[3] + 1):
                        if val not in arr:
                            arr.append(val)

    def _enc_prefix(self, count, bytes):
        if count == 2:
            if bytes[0] not in self._two_byte.keys():
                self._two_byte[bytes[0]] = []
        elif count == 3:
            if bytes[0] not in self._three_byte.keys():
                self._three_byte[bytes[0]] = {}
            if bytes[1] not in self._three_byte[bytes[0]].keys():
                self._three_byte[bytes[0]][bytes[1]] = []
        elif count == 4:
            if bytes[0] not in self._four_byte.keys():
                self._four_byte[bytes[0]] = {}
            if bytes[1] not in self._four_byte[bytes[0]].keys():
                self._four_byte[bytes[0]][bytes[1]] = {}
            if bytes[2] not in self._four_byte[bytes[0]][bytes[1]].keys():
                self._four_byte[bytes[0]][bytes[1]][bytes[2]] = []

    def dump(self, fp):
        pass

    def generate_tree_declaration(self, fp, symbol):
        pass

    def generate_tree_definition(self, fp, symbol):
        pass


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

    # for group in properties:
    #     group.generate_class_def(fp)

    eprint("Generating unicode charclass tree...")

    for group in properties:
        eprint("    Generating tree for {}...".format(group.unicode_class()))
        tree = UnicodeByteTree(group)

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
