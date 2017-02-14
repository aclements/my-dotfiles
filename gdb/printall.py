import gdb
import re

# XXX: This gets things like x[*].x - x[*].y wrong because it doesn't
# understand the associativity of '-' versus '.'. Currently this needs
# to be parenthesized.

# TODO: Support pointer chasing, e.g., list.next*.x

# TODO: Support nested wildcards. Maybe "*", "**", etc.

# TODO: Support specifying a particular range.

# TODO: Support / format specifiers.

class PrintAll(gdb.Command):
    """print-all expr: print all values of expr, where expr may contain '[*]' wildcard indexes."""

    wrappers = []

    def __init__(self):
        super(PrintAll, self).__init__("print-all", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        # Lex the expression enough to get high-level structure.
        OP = r"][*.()"
        STRING = r'"(?:\\.|[^\\"])*"|' + r"'(?:\\.|[^\\'])*'"
        toks = re.findall(r"[" + OP + r"]|" + STRING + r"|[^" + OP + r"\s]+", arg)

        self.__expand(toks, from_tty)

    def __expand(self, toks, from_tty):
        wildcards = self.__find(toks, "*")

        if not wildcards:
            # No wildcard. Evaluate directly.
            expr = "".join(toks)
            sys.stdout.write("%s = " % expr)
            gdb.execute("output " + expr, from_tty)
            sys.stdout.write("\n")
            return

        # Evaluate the wildcarded subexpressions.
        subtoks = ["".join(toks[l:r]) for (l, r) in wildcards]
        subexprs = [self.__wrap(gdb.parse_and_eval(subtok), subtok) for subtok in subtoks]

        # Check that they all have the same range.
        r = (subexprs[0].lo, subexprs[0].hi)
        for i, subexpr in enumerate(subexprs):
            r2 = (subexpr.lo, subexpr.hi)
            if r != r2:
                raise gdb.GdbError("%s has range %s, but %s has range %s" % (subtoks[0], r, subtoks[i], r2))

        # Evaluate across the range.
        for i in range(r[0], r[1] + 1):
            etoks = toks[:]
            for (wi, (l, r)) in reversed(list(enumerate(wildcards))):
                # Fill in index for printing.
                toks[r+1] = "%s" % i
                etoks[r+1] = "%s" % i
                # Insert expression to index, which might be different
                # from the original expression.
                etoks[l:r] = subexprs[wi].expr
            sys.stdout.write("%s = " % "".join(toks))
            gdb.execute("output " + "".join(etoks), from_tty)
            sys.stdout.write("\n")

    def __find(self, toks, wildcard):
        # Find the wildcard and keep track of the parenthesized
        # expression containing it.
        wildcards = []
        pstack = [0]
        for i in range(len(toks)-2):
            if toks[i:i+3] == ["[", wildcard, "]"]:
                wildcards.append((pstack[-1], i))
            elif toks[i] == "(":
                pstack.append(i+1)
            elif toks[i] == ")":
                if len(pstack) > 1:
                    pstack.pop()
        return wildcards

    def __wrap(self, val, expr):
        for tryWrap in PrintAll.wrappers:
            wrapped = tryWrap(val, expr)
            if wrapped is not None:
                return wrapped
        raise gdb.GdbError("don't know how to iterate over %s" % expr)

class ArrayWrapper:
    """Wrapper for statically sized array values."""

    def __init__(self, val, expr, lo, hi):
        self.expr = expr
        self.lo, self.hi = lo, hi

    @classmethod
    def tryWrap(cls, val, expr):
        # Statically-sized arrays.
        if val.type.code == gdb.TYPE_CODE_ARRAY and val.type.sizeof != 0:
            return cls(val, expr, *val.type.range())
        # Go slices.
        if val.type.code == gdb.TYPE_CODE_STRUCT:
            try:
                array, alen = val["array"], val["len"]
            except gdb.error:
                pass
            else:
                return cls(array, "(" + expr + ").array", 0, alen - 1)

PrintAll.wrappers.append(ArrayWrapper.tryWrap)

PrintAll()
