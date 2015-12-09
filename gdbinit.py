from __future__ import print_function

import gdb
try:
    from gdb.unwinders import Unwinder
except ImportError:
    Unwinder = None

def command(fn):
    name = fn.__name__.replace('_', '-')
    dct = {
        '__doc__': fn.__doc__,
        '__init__': lambda self: super(cls, self).__init__(name, gdb.COMMAND_USER),
        'invoke': lambda self, *args, **kw: fn(*args, **kw),
    }
    cls = type(fn.__name__ + 'Cls', (gdb.Command,), dct)
    cls()
    return fn

#
# General commands
#

@command
def afind(arg, from_tty):
    """afind array, val[, val...]: find instances of val in array

val may be "!pyexr", where pyexr will be evaluated with x bound to
each array value."""

    args = arg.split(',')
    if len(args) < 2:
        raise gdb.GdbError('afind requires at least two arguments')
    array = gdb.parse_and_eval(args[0])
    preds = []
    for varg in args[1:]:
        if varg.lstrip().startswith('!'):
            varg = compile(varg.lstrip()[1:], '<arg>', 'eval', dont_inherit=True)
            preds.append(lambda x, varg=varg: eval(varg, None, {'x': x}))
        else:
            varg = gdb.parse_and_eval(varg)
            preds.append(lambda x, varg=varg: x==varg)
    for i in range(*array.type.range()):
        for p in preds:
            if p(array[i]):
                print('[%d]=%s' % (i, str(array[i])))

#
# Go runtime helpers
#

class SliceValue:
    """Wrapper for slice values."""

    def __init__(self, val):
        self.val = val

    @property
    def len(self):
        return int(self.val['len'])

    @property
    def cap(self):
        return int(self.val['cap'])

    def __getitem__(self, i):
        if i < 0 or i >= self.len:
            raise IndexError(i)
        ptr = self.val["array"]
        return (ptr + i).dereference()

_Gdead = 6

def getg(sp=None, n=None):
    if sp is None and n is None:
        sp = gdb.parse_and_eval('$sp')

    found = [None]
    def checkg(gp):
        if gp == 0:
            return
        if gp['atomicstatus'] == _Gdead:
            return
        if sp != None and gp['stack']['lo'] < sp and sp <= gp['stack']['hi']:
            if found[0] is not None:
                raise gdb.GdbError('multiple Gs with overlapping stacks!')
            found[0] = gp
        if n != None and gp['goid'] == n:
            if found[0] is not None:
                raise gdb.GdbError('multiple Gs with same goid!')
            found[0] = gp

    # Check allgs.
    for gp in SliceValue(gdb.parse_and_eval("'runtime.allgs'")):
        checkg(gp)

    # Check g0s and gsignals, which aren't on allgs.
    if sp is not None:
        mp = gdb.parse_and_eval("'runtime.allm'")
        while mp != 0:
            checkg(mp['g0'])
            checkg(mp['gsignal'])
            mp = mp['alllink']

    return found[0]

class GetG(gdb.Function):
    """Return the current *g."""

    def __init__(self):
        super(GetG, self).__init__("getg")

    def invoke(self, n=None):
        g = getg(n=n)
        if g is None:
            raise gdb.GdbError('G not found')
        return g
GetG()

class GetGBySP(gdb.Function):
    """Return the *g for a stack pointer."""

    def __init__(self):
        super(GetGBySP, self).__init__("getgbysp")

    def invoke(self, sp):
        return getg(sp)
GetGBySP()

if Unwinder is not None:
    # TODO: This has not been tested because I don't have GDB 7.10.
    class FrameID(object):
        def __init__(self, sp, pc):
            self.sp = sp
            self.pc = pc

    class BTGUnwinder(Unwinder):
        def __init__(self):
            super(BTGUnwinder, self).__init___("btg-unwinder")

        def __call__(pending_frame):
            # This only applies to the first frame.
            self.enabled = False

            # Ignore registers in pending_frame. Use stashed PC/SP.
            return pending_frame.create_unwind_info(self.frame_id)

@command
def btg(arg, from_tty):
    """btg g: print a backtrace for G."""

    if not arg.strip():
        gdb.execute('backtrace')
        return

    g = gdb.parse_and_eval(arg)
    # TODO: LR register on LR machines.
    if g['syscallsp'] != 0:
        sp, pc = g['syscallsp'], g['syscallpc']
    else:
        sp, pc = g['sched']['sp'], g['sched']['pc']

    if Unwinder is None:
        # This does not require the new unwinder functionality, but it
        # doesn't work on core dumps.
        oldsp, oldpc = gdb.parse_and_eval('$sp'), gdb.parse_and_eval('$pc')
        try:
            gdb.execute('set $sp = %#x' % sp)
            gdb.execute('set $pc = %#x' % pc)
            gdb.execute('backtrace')
        finally:
            gdb.execute('set $sp = %#x' % oldsp)
            gdb.execute('set $pc = %#x' % oldpc)
    else:
        btgUnwinder.frame_id = FrameID(sp, pc)
        btgUnwinder.enabled = True
        try:
            gdb.execute('backtrace')
        finally:
            btgUnwinder.enabled = False

#
# Memory manager debugging helpers
#

ptrSize = int(gdb.lookup_type("void").pointer().sizeof)
pageShift = 13
_MSpanInUse = 0

def bitmapOf(addr):
    addr = long(addr)
    arenaStart = int(gdb.parse_and_eval("'runtime.mheap_'.arena_start"))
    off = (addr - arenaStart) / ptrSize
    ptrChar = gdb.lookup_type('char').pointer()
    return gdb.Value(arenaStart - off / 4 - 1).cast(ptrChar), off & 3

class BitmapOf(gdb.Function):
    """Return the address of the heap bitmap of x."""

    def __init__(self):
        super(BitmapOf, self).__init__("bitmapOf")

    def invoke(self, addr):
        base, bit = bitmapOf(addr)
        print("offset %d" % bit)
        return base
BitmapOf()

def spanOf(addr):
    addr = int(addr)
    arenaStart = int(gdb.parse_and_eval("'runtime.mheap_'.arena_start"))
    h_spans = gdb.parse_and_eval("'runtime.h_spans'")
    return h_spans['array'][(addr - arenaStart) >> pageShift]

class SpanOf(gdb.Function):
    """Return the *mspan of x."""
    
    def __init__(self):
        super(SpanOf, self).__init__("spanOf")

    def invoke(self, addr):
        return spanOf(addr)
SpanOf()

@command
def dump_bitmap(arg, from_tty):
    """dump-bitmap[/words] ptr: dump the object and bitmap at ptr."""

    if arg.startswith('/'):
        count, arg = arg[1:].split(None, 1)
        count = int(count)
    else:
        count = None

    base = int(gdb.parse_and_eval(arg))
    objStart = None
    span = spanOf(base)
    if span and span['state'] == _MSpanInUse:
        spanStart = span['start'] << pageShift
        objStart = int((base - spanStart) / span['elemsize'] * span['elemsize'] + spanStart)
        if count is None:
            if objStart >= span['limit']:
                raise gdb.GdbError('address past end of span; must specify a size')
            base = objStart
            count = int(span['elemsize'] / ptrSize)

    if count is None:
        count = 1

    uintptr = gdb.lookup_type('uintptr')
    ptrChar = gdb.lookup_type('char').pointer()
    nibbles = ptrSize * 2
    for addr in range(base, base+count*ptrSize, ptrSize):
        bmptr, bit = bitmapOf(addr)
        bm = bmptr.dereference()
        typ = 'pointer' if (bm & (1<<bit)) else 'scalar'
        if addr == objStart:
            mark = 'marked' if (bm & (1<<(bit+4))) else 'unmarked'
        elif objStart != None and addr == objStart + ptrSize:
            mark = 'checkmarked' if (bm & (1<<(bit+4))) else 'uncheckmarked'
        else:
            mark = 'scan' if (bm & (1<<(bit+4))) else 'dead'

        val = gdb.Value(addr).cast(uintptr.pointer()).dereference()
        print("%0*x: %0*x    %0*x/%d: %-7s %s" % (nibbles, addr, nibbles, val, nibbles, long(bmptr), bit, typ, mark))


#
# cmd/link debugging helpers
#

@command
def dump_allsym(arg, from_tty):
    """dump-allsym: walk Ctxt.Allsym list."""

    s = gdb.parse_and_eval("'cmd/link/internal/ld.Ctxt'.Allsym")
    i = 0
    f = open('/tmp/dump_allsym', 'w')
    while s:
        bmptr, bit = bitmapOf(s)
        val = int(bmptr.dereference())
        m = "marked" if val & (1<<4) else "unmarked"
        print("%d: %s %s" % (i, str(s.dereference().address), m), file=f)
        i += 1
        s = s['Allsym']

@command
def check_allsym(arg, from_tty):
    """check-allsym: print LSyms with bad bitmaps."""
    s = gdb.parse_and_eval("'cmd/link/internal/ld.Ctxt'.Allsym")
    while s:
        bmptr, bit = bitmapOf(s)
        val = int(bmptr.dereference())
        if not (val == 0xd5 or val == 0xc5):
            print(hex(long(s)), hex(val))
        s = s['Allsym']

#
# Issue 11911
#

@command
def dump_assist_state(arg, from_tty):
    """dump-assist-state: print the assist state of all Gs."""
    total_gcalloc = total_gcscanwork = 0
    ar = gdb.parse_and_eval("'runtime.gcController'.assistRatio")
    for gp in SliceValue(gdb.parse_and_eval("'runtime.allgs'")):
        print(gp['atomicstatus'], gp['gcalloc'], gp['gcalloc']*ar, gp['gcscanwork'])
        total_gcalloc += gp['gcalloc']
        total_gcscanwork += gp['gcscanwork']

    print('total:', total_gcalloc, total_gcalloc*ar, total_gcscanwork)
