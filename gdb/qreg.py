from __future__ import print_function

import gdb

# XXX Highlight changed registers
#
# XXX Pretty-printing of rip, eflags, pc, lr

class QregCmd(gdb.Command):
    """Show all registers a compact form."""

    def __init__(self):
        super(QregCmd, self).__init__('qreg', gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        if arg.strip():
            raise gdb.GdbError('qreg takes no arguments')

        arch = gdb.newest_frame().architecture().name()
        meth = '_do_' + arch.replace(':', '_').replace('-', '_')
        if hasattr(self, meth):
            getattr(self, meth)()
        else:
            # XXX Could just reformat the output of info reg
            raise gdb.GdbError('unsupported architecture: %s' % arch)

    def _do_i386_x86_64(self):
        regs = [('$rax', 'rax'), ('$rbx', ' b'), ('$rcx', ' c'), ('$rdx', ' d'),
                ('$rsi', 'rsi'), ('$rdi', 'di'), ('$rbp', 'bp'), ('$rsp', 'sp'),
                ('$r8',  ' r8'), ('$r9 ', ' 9'), ('$r10', '10'), ('$r11', '11'),
                ('$r12', 'r12'), ('$r13', '13'), ('$r14', '14'), ('$r15', '15'),
                '$rip', ('$eflags', 'eflags', 8)]
        self.__dump(regs, 16)

    def _do_powerpc_common64(self):
        regs = [('$r%d' % n, ('r%d' % n).rjust(3) if n % 4 == 0 else '')
                for n in range(32)]
        regs.extend([('$pc', ' pc'), '$msr', ('$cr', ' cr'),
                     ('$lr', ' lr'), '$ctr', '$xer', '$trap'])
        self.__dump(regs, 16)

    def __dump(self, regs, width):
        lines = ['']
        for spec in regs:
            w = width
            if isinstance(spec, basestring):
                var = spec
                label = spec.lstrip('$')
            else:
                var, label = spec[:2]
                if len(spec) == 3:
                    w = spec[2]

            gval = gdb.parse_and_eval(var)
            val = long(gval)
            if val < 0:
                # Blech.  Twos-complement it back to the real bits.
                flip = (1 << (gval.type.sizeof * 8)) - 1
                val = (-val ^ flip) + 1

            if label:
                seg = label + ' '
            else:
                seg = ''
            seg += '%0*x' % (w, val)
            if len(lines[-1]) + 1 + len(seg) > 80:
                lines.append('')
            if lines[-1]:
                lines[-1] += ' '
            lines[-1] += seg
        for l in lines:
            print(l)

QregCmd()
