# Compute the object graph of a Go heap and find the path from a root
# to a target address.

CONSERVATIVE_STACKS = False

import gdb
import collections
import time
import sys

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

def iterlist(x, link="next"):
    while x != 0:
        yield x
        x = x[link]

# Old states:
#_MSpanInUse = 0
#_MSpanStack = 1

_MSpanDead, _MSpanInUse, _MSpanStack, _MSpanFree = range(4)

_Gdead = 6

_KindSpecialFinalizer = 1

ptrSize = 8

_PageShift = 13

def heapSpans():
    """Yield in-use heap spans."""
    for span in SliceValue(gdb.parse_and_eval("'runtime.mheap_'.allspans")):
        if span['state'] == _MSpanInUse:
            yield span

# def spanobjs(span):
#     """Yield addresses of allocated objects in span."""
#     base = span['startAddr']
#     elemsize = span['elemsize']
#     nelems = span['nelems']
#     freeindex = span['freeindex']
#     abits = span['allocBits']
#     for i in range(0, nelems):
#       if i < freeindex or abits[i/8]&(1<<(i%8)) != 0:
#           yield base + i * elemsize

class AddrSpace(object):
    def __init__(self):
        # Map from base address to Obj.
        self.objs = {}
        self.arena = (long(gdb.parse_and_eval("'runtime.mheap_'.arena_start")),
                      long(gdb.parse_and_eval("'runtime.mheap_'.arena_used")))
        self.uintptr_p = gdb.lookup_type("uintptr").pointer()
        self.hspans = SliceValue(gdb.parse_and_eval("'runtime.mheap_'.spans"))
        bytep = gdb.lookup_type("uint8").pointer()
        self.bitmap = gdb.parse_and_eval("'runtime.mheap_'.bitmap").cast(bytep)

    def readWord(self, p):
        return long(gdb.Value(p).cast(self.uintptr_p).dereference())

    def obj(self, base, *args):
        base = long(base)
        if base in self.objs:
            return self.objs[base]
        obj = Obj(base, *args)
        self.objs[base] = obj
        return obj

    def heapObj(self, ptr, name):
        ptr = long(ptr)
        if ptr in self.objs:
            return self.objs[ptr]
        span = self.spanOf(ptr)
        len = span["elemsize"]
        base = long((ptr - span["startAddr"]) / len * len + span["startAddr"])
        if base in self.objs:
            return self.objs[base]
        return self.obj(base, len, name, HeapBitmap(self, base, len))

    def spanOf(self, b):
        if b < self.arena[0] or b >= self.arena[1]:
            return None
        s = self.hspans[(b - self.arena[0]) >> _PageShift]
        if s == 0:
            return None
        return s

    def inheap(self, b):
        s = self.spanOf(b)
        if s == None or b >= s["limit"] or s["state"] != _MSpanInUse:
            return False
        return True

# Bitmap-based object
class Obj(object):
    def __init__(self, base, len, name, bitmap):
        self.base, self.len, self.name, self.bitmap = base, len, name, bitmap
        #self.marked = False
        self.marked = 0
        self.parent = None

    def __repr__(self):
        return "Obj(%#x, %d, %s, %s)" % (self.base, self.len, self.name, self.bitmap)

    def __str__(self):
        s = "%#x" % self.base
        if self.name:
            s += " " + self.name
        s += " [%d bytes]" % self.len
        return s

    def children(self, addrspace):
        if not self.bitmap.hasPointers():
            return
        for word in range(self.len / ptrSize):
            if self.bitmap[word]:
                #val = long(gdb.Value(self.base + word * ptrSize).cast(addrspace.uintptr_p).dereference())
                val = addrspace.readWord(self.base + word * ptrSize)
                if not addrspace.inheap(val):
                    continue
                yield (word * ptrSize, addrspace.heapObj(val, ""))

class OneBitBitmap(object):
    def __init__(self, bitmap, startBit):
        self.bitmap, self.startBit = bitmap, startBit

    def __getitem__(self, i):
        bit = self.startBit + i
        return bool(self.bitmap[bit / 8] & (1 << (bit % 8)))

    def hasPointers(self):
        return True

class ConstBitmap(object):
    def __init__(self, bits, len):
        self.bits, self.len = bits, len

    def repeat(self, n):
        bits, len = self.bits, self.len
        while len <= n * self.len:
            bits |= bits << len
            len *= 2
        len = n * self.len
        bits &= (1 << len) - 1
        return ConstBitmap(bits, len)

    def __getitem__(self, i):
        if not (0 <= i < self.len):
            raise IndexError()
        return bool((self.bits >> i) & 1)

    def __repr__(self):
        return "ConstBitmap(%#x, %d)" % (self.bits, self.len)

    def __str__(self):
        return "0b" + bin(self.bits)[2:].rjust(self.len, "0")

    def hasPointers(self):
        return self.bits != 0

class HeapBitmap(object):
    def __init__(self, addrspace, base, len):
        # XXX Heap bitmap is not initialized for noscan objects.
        off = (base - addrspace.arena[0]) / ptrSize
        self.bitp = addrspace.bitmap - off / 4 - 1
        self.shift = off & 3
        self.len = len

    def hasPointers(self):
        if self.len == ptrSize:
            return True
        shift = self.shift
        bitp = self.bitp - shift / 4
        shift = shift % 4
        return bool((bitp.dereference() >> shift) & (1 << 4))

    def __getitem__(self, i):
        if not (0 <= i < self.len):
            raise IndexError()
        shift = self.shift + i
        bitp = self.bitp - shift / 4
        shift = shift % 4
        return bool((bitp.dereference() >> shift) & 1)

def typeBitmap(typ):
    typ = typ.strip_typedefs()
    words = typ.sizeof / ptrSize

    if typ.code == gdb.TYPE_CODE_PTR:
        if words != 1:
            raise ValueError("pointer %s has wrong size %d" % (typ, typ.sizeof))
        return ConstBitmap(1, 1)
    if typ.code == gdb.TYPE_CODE_FUNC:
        # This has size 1 for some reason.
        return ConstBitmap(1, 1)
    if typ.code == gdb.TYPE_CODE_ARRAY:
        lo, hi = typ.range()
        return typeBitmap(typ.target()).repeat(hi - lo)

    bitmap = 0
    if typ.code == gdb.TYPE_CODE_STRUCT:
        for f in typ.fields():
            fbitmap = typeBitmap(f.type)
            start = f.bitpos / 8 / ptrSize
            bitmap |= fbitmap.bits << start
        return ConstBitmap(bitmap, words)
    if typ.code in (gdb.TYPE_CODE_INT, gdb.TYPE_CODE_FLT,
                    gdb.TYPE_CODE_CHAR, gdb.TYPE_CODE_BOOL):
        return ConstBitmap(0, words)

    raise ValueError("Unhandled type code %d (%s)" % (typ.code, typ))

# Type-based object
#
# Doesn't work because for global roots, we only know the DWARF types,
# but for interfaces, we only know the Go types.

# class Obj(object):
#     def __init__(self, val, name):
#       self.val, self.name = val, name
#       self.marked, self.parent = False, None

#     def __repr__(self):
#       return "Obj(%r, %r)" % (self.val, self.name)

#     def children(self, addrspace):
#       children = []
#       def rec(val, name):
#           typ = val.type.strip_typedefs()
#           # TODO: gdb seems confused by our func pointers. It thinks
#           # they're 1 byte and dereference just seems to return the
#           # same thing.
#           if typ.code in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_FUNC):
#               print typ, self.val.type
#               children.append(addrspace.obj(val.dereference(), "*" + name))
#           elif typ.code == gdb.TYPE_CODE_ARRAY:
#               for i in range(*typ.range()):
#                   rec(val[i], "%s[%d]" % (name, i))
#           elif typ.code == gdb.TYPE_CODE_STRUCT:
#               for f in typ.fields():
#                   rec(val[f.name], "%s.%s" % (name, f.name))
#           elif typ.code in (gdb.TYPE_CODE_INT, gdb.TYPE_CODE_FLT,
#                             gdb.TYPE_CODE_CHAR, gdb.TYPE_CODE_BOOL):
#               return
#           else:
#               raise ValueError("Unhandled type code %d (%s)" % (typ.code, typ))
#       rec(self.val, self.name)
#       return children

def roots(addrspace):
    # Globals
    gbitmaps = []
    bytep = gdb.lookup_type("uint8").pointer()
    for md in iterlist(gdb.parse_and_eval("&'runtime.firstmoduledata'")):
        gbitmaps.append((long(md["data"]), long(md["edata"]), md["gcdata"].cast(bytep)))
        gbitmaps.append((long(md["bss"]), long(md["ebss"]), md["gcbss"].cast(bytep)))
    for sym in gdb.selected_frame().block().global_block:
        if not sym.is_variable:
            continue
        addr = long(sym.value().address)
        # Find sym's bitmap
        bitmap = None
        for sbase, send, sbitmap in gbitmaps:
            if sbase <= addr < send:
                bitmap = OneBitBitmap(sbitmap, (addr - sbase) / ptrSize)
                break
        if bitmap is None:
            # Could be rodata, noptrdata, etc.
            continue
        yield addrspace.obj(addr, sym.type.sizeof, sym.name, bitmap)
        #yield addrspace.obj(sym.value(), sym.name)

    # Stacks of running Gs
    tidToThr = {}
    for thr in gdb.selected_inferior().threads():
        pid, lwpid, tid = thr.ptid
        tidToThr[lwpid] = thr
    haveGs = set()
    curThr = gdb.selected_thread()
    try:
        for mp in iterlist(gdb.parse_and_eval("'runtime.allm'"), "alllink"):
            thr = tidToThr.get(long(mp["procid"]), None)
            if thr is None:
                continue
            thr.switch()
            curg = mp["curg"]
            if curg == 0:
                continue
            sp = gdb.parse_and_eval("$sp")
            if not (curg["stack"]["lo"] < sp <= curg["stack"]["hi"]):
                # We're on the g0 and curg's state is in sched.
                continue
            if CONSERVATIVE_STACKS:
                for root in conservativeStackRoots(addrspace, sp, curg["stack"]["hi"]):
                    yield root
            else:
                for root in stackRoots(addrspace):
                    yield root
            haveGs.add(curg["goid"])
    finally:
        curThr.switch()

    # Stacks of non-running Gs
    for g in SliceValue(gdb.parse_and_eval("'runtime.allgs'")):
        if g["goid"] in haveGs or g["atomicstatus"] == _Gdead:
            continue

        if g['syscallsp'] != 0:
            sp, pc = g['syscallsp'], g['syscallpc']
        else:
            sp, pc = g['sched']['sp'], g['sched']['pc']
        if sp == 0:
            # TODO: When does this happen?
            continue

        if CONSERVATIVE_STACKS:
            for root in conservativeStackRoots(addrspace, sp, g['stack']['hi']):
                yield root
        else:
            # XXX Doesn't work on core files. :(
            oldsp, oldpc = gdb.parse_and_eval('$sp'), gdb.parse_and_eval('$pc')
            try:
                # TODO: This fails if we're not in the innermost frame.
                gdb.execute('set $sp = %#x' % sp)
                gdb.execute('set $pc = %#x' % pc)
                for root in stackRoots(addrspace):
                    yield root
            finally:
                gdb.execute('set $sp = %#x' % oldsp)
                gdb.execute('set $pc = %#x' % oldpc)

    # Span specials
    for s in heapSpans():
        for sp in iterlist(s["specials"]):
            if sp["kind"] != _KindSpecialFinalizer:
                continue
            obj = long(s["startAddr"] + sp["offset"]/s["elemsize"]*s["elemsize"])
            yield addrspace.obj(obj, s["elemsize"], "finalized object", HeapBitmap(addrspace, obj, s["elemsize"]))
            # TODO: Finalizer function

    # TODO: Finalizer queue

def stackRoots(addrspace):
    """Yield the roots from the current stack."""
    fr = gdb.newest_frame()
    while fr:
        block = fr.block()
        while block.function != None:
            for sym in block:
                val = sym.value(fr)
                name = "%s[%s]" % (fr.name(), sym.name.decode("utf8"))
                base = long(val.address)
                #print hex(base), name
                #if addrspace.inheap(base):
                yield addrspace.obj(base, sym.type.sizeof, name, typeBitmap(sym.type))
                #yield addrspace.obj(val, name)
            block = block.superblock
        if fr.name() == "runtime.goexit":
            # GDB thinks it can backtrace past here, but just gets
            # confused.
            break
        fr = fr.older()

def conservativeStackRoots(addrspace, sp, hi):
    #print(str(sp), str(hi))
    while sp < hi:
        word = addrspace.readWord(sp)
        if addrspace.inheap(word):
            yield addrspace.heapObj(word, "stack")
        sp += ptrSize

def computeParents():
    lastReport = 0
    addrspace = AddrSpace()
    q = collections.deque(roots(addrspace))
    for obj in q:
        #obj.marked = True
        obj.marked += 1
    scanned = set()
    scannedBytes, scannedObjects = 0, 0
    while len(q):
        now = time.time()
        if now - lastReport > 1:
            lastReport = now
            sys.stdout.write("scanned %d bytes, %d objects, queued %d\r" % (scannedBytes, scannedObjects, len(q)))
            sys.stdout.flush()

        parent = q.popleft()
        if parent in scanned:
            # WHY IS THIS HAPPENING?
            continue
        scanned.add(parent)
        scannedBytes += parent.len
        scannedObjects += 1
        #print id(parent), parent.base, parent.len, parent.marked
        for offset, obj in parent.children(addrspace):
            if obj.parent == None:
                # Record parents for roots too, to help find cycles.
                obj.parent = (parent, offset)
            if not obj.marked:
                #obj.marked = True
                obj.marked += 1
                q.append(obj)
    sys.stdout.write("\x1b[2K")
    sys.stdout.flush()
    return addrspace

class FindPath(gdb.Command):
    """find-path obj: print the path from a GC root to obj"""
    def __init__(self):
        super(FindPath, self).__init__("find-path", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        target = gdb.parse_and_eval(arg)
        if target.type.code == gdb.TYPE_CODE_PTR:
            target = long(target.dereference().address)
        else:
            target = long(target)
        addrspace = computeParents()

        o = addrspace.objs.get(target)
        print o
        while o.parent:
            o, offset = o.parent
            print o, "+", offset
FindPath()

class ObjOf(gdb.Command):
    """obj-of ptr: print information about the object containing ptr"""
    def __init__(self):
        super(ObjOf, self).__init__("obj-of", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        target = gdb.parse_and_eval(arg)
        if target.type.code == gdb.TYPE_CODE_PTR:
            target = long(target.dereference().address)
        else:
            target = long(target)
        addrspace = AddrSpace()
        span = addrspace.spanOf(target)
        if span["state"] == _MSpanInUse:
            len = addrspace.spanOf(target)["elemsize"]
            base = (target - span["startAddr"]) / len * len + span["startAddr"]
            print "Base:", hex(long(base))
            print "Len:", len
            print "Span:", span
        elif span["state"] == _MSpanStack:
            print "In stack span"
        else:
            print "Span state:", span["state"]
ObjOf()
