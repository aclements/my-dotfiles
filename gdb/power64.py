import struct

def select_table(l, r, opts):
    """Return f(i) that returns opts[x] where x is bits [l:r] of i."""
    shift = 31 - r
    mask = (1 << (r - l + 1)) - 1
    return lambda i: opts.get((i >> shift) & mask)

def selector32(l, r, ext=False):
    shift = 31 - r
    mask = (1 << (r - l + 1)) - 1
    if not ext:
        return lambda val: (val >> shift) & mask
    top = mask & ~(mask >> 1)
    def s(val):
        val = (val >> shift) & mask
        if val & top:
            val = -((val ^ mask) + 1)
        return val
    return s

def d_addr(width):
    """D-form memory load/store of width."""
    sra = selector32(11, 15)
    sd = selector32(16, 31, True)
    def inst(inst, regs):
        ra = sra(inst)
        b = 0 if ra == 0 else regs[ra]
        ea = b + sd(inst)
        return ea, width
    return inst

def ds_addr(width):
    """DS-form memory load/store of width."""
    sra = selector32(11, 15)
    sd = selector32(16, 29, True)
    def inst(inst, regs):
        ra = sra(inst)
        b = 0 if ra == 0 else regs[ra]
        ea = b + (sd(inst) << 2)
        return ea, width
    return inst

def x_addr(width):
    """X-form memory load/store of width."""
    sra = selector32(11, 15)
    srb = selector32(16, 20)
    def inst(inst, regs):
        ra = sra(inst)
        b = 0 if ra == 0 else regs[ra]
        ea = b + regs[srb(inst)]
        return ea, width
    return inst

# Not implemented: Load/store quadword instructions, load/store with
# byte reversal, load/store multiple, move assist, floating-point
# load/store doubleword pair, vector load instructions, VSX
# instructions, transactional memory facility, fixed-point load and
# store caching inhibited
INSTRUCTIONS = select_table(0, 5, {
    31:  select_table(21, 30, {
        20:  ('lwarx', x_addr(4), None),
        21:  ('ldx',   x_addr(8), None),
        52:  ('lbarx', x_addr(1), None),
        53:  ('ldux',  x_addr(8), None),
        23:  ('lwzx',  x_addr(4), None),
        55:  ('lwzux', x_addr(4), None),
        84:  ('ldarx', x_addr(8), None),
        87:  ('lbzx',  x_addr(1), None),
        116: ('lharx', x_addr(2), None),
        119: ('lbzux', x_addr(1), None),
        149: ('stdx',  None, x_addr(8)),
        150: select_table(31, 31, {1: ('stwcx', None, x_addr(4))}),
        151: ('stwx',  None, x_addr(4)),
        181: ('stdux', None, x_addr(8)),
        182: select_table(31, 31, {1: ('stqcx', None, x_addr(16))}),
        183: ('stwux', None, x_addr(4)),
        214: select_table(31, 31, {1: ('stdcx', None, x_addr(8))}),
        215: ('stbx',  None, x_addr(1)),
        247: ('stbux', None, x_addr(1)),
        267: ('lqarx', x_addr(16), None),
        279: ('lhzx',  x_addr(2), None),
        311: ('lhzux', x_addr(2), None),
        341: ('lwax',  x_addr(4), None),
        343: ('lhax',  x_addr(2), None),
        373: ('lwaux', x_addr(4), None),
        375: ('lhaux', x_addr(2), None),
        407: ('sthx',  None, x_addr(2)),
        439: ('sthux', None, x_addr(2)),
        535: ('lfsx',  x_addr(4), None),
        567: ('lfsix', x_addr(4), None),
        599: ('lfdx',  x_addr(8), None),
        631: ('lfdux', x_addr(8), None),
        663: ('stfsx', None, x_addr(4)),
        694: select_table(31, 31, {1: ('stbcx', None, x_addr(1))}),
        695: ('stfsux',None, x_addr(4)),
        726: select_table(31, 31, {1: ('sthcx', None, x_addr(2))}),
        727: ('stfdx', None, x_addr(8)),
        759: ('stfdux',None, x_addr(8)),
        855: ('lfiwax',x_addr(4), None),
        887: ('lfiwzx',x_addr(4), None),
        983: ('stfiwx',None, x_addr(4)),
    }),
    32:  ('lwz',  d_addr(4), None),
    33:  ('lwzu', d_addr(4), None),
    34:  ('lbz',  d_addr(1), None),
    35:  ('lbzu', d_addr(1), None),
    36:  ('stw',  None, d_addr(4)),
    37:  ('stwu', None, d_addr(4)),
    38:  ('stb',  None, d_addr(1)),
    39:  ('stbu', None, d_addr(1)),
    40:  ('lhz',  d_addr(2), None),
    41:  ('lhzu', d_addr(2), None),
    42:  ('lha',  d_addr(2), None),
    43:  ('lhau', d_addr(2), None),
    44:  ('sth',  None, d_addr(2)),
    45:  ('sthu', None, d_addr(2)),
    48:  ('lfs',  d_addr(4), None),
    49:  ('lfsu', d_addr(4), None),
    50:  ('lfd',  d_addr(8), None),
    51:  ('lfdu', d_addr(8), None),
    52:  ('stfs', None, d_addr(4)),
    53:  ('stfsu',None, d_addr(4)),
    54:  ('stfd', None, d_addr(8)),
    55:  ('stfdu',None, d_addr(8)),
    58:  select_table(30, 31, {
        0: ('ld',   ds_addr(8), None),
        1: ('ldu',  ds_addr(8), None),
        2: ('lwa',  ds_addr(4), None),
    }),
    62:  select_table(30, 31, {
        0: ('std',  None, ds_addr(8)),
        1: ('stdu', None, ds_addr(8)),
    }),
})

def decode(inst):
    table = INSTRUCTIONS
    while callable(table):
        table = table(inst)
    return table

def addr_decode(inst, regs):
    # XXX Determine endianness
    (inst,) = struct.unpack('<I', inst)
    row = decode(inst)
    if row is None:
        return (None, None)
    op, r, w = row
    return r(inst, regs) if r else None, w(inst, regs) if w else None

# Basic tests
# stdu r31,-8(r1)
assert addr_decode(b'\xf9\xff\xe1\xfb', [0, 20]) == (None, (12, 8))
# stw r3,8(r1)
assert addr_decode(b'\x08\x00\x61\x90', [0, 20]) == (None, (28, 4))
# lwa r6,48(r1)
assert addr_decode(b'\x32\x00\xc1\xe8', [0, 20]) == ((68, 4), None)
