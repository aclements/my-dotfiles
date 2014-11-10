from __future__ import print_function

import gdb

import os, sys
# XXX There has to be a better way
sys.path.append(os.path.abspath(os.path.dirname(os.path.expanduser(__file__))))
import power64

ARCH_ADDRS = {
    'powerpc:common64': power64.addr_decode,
}

def hexmem(mem):
    return ' '.join('%02x' % ord(b) for b in mem)

class TraceiCmd(gdb.Command):
    """Record an instruction trace."""

    def __init__(self):
        super(TraceiCmd, self).__init__('tracei', gdb.COMMAND_USER)
        self.__regNames = None

    def __get_regs(self):
        regs = gdb.execute('info registers', to_string=True).splitlines()
        if self.__regNames is None:
            self.__regNames = [reg.split(None, 1)[0] for reg in regs]
        return [int(reg.split(None, 2)[1], 0) for reg in regs]

    def __log_mem(self, label, addr_width):
        if addr_width is None:
            return
        try:
            mem = self.__inferior.read_memory(*addr_width)
        except gdb.MemoryError as e:
            print('%s %#016x %s' % (label, addr_width[0], e),
                  file=self.__logFile)
        else:
            print('%s %#016x %s' % (label, addr_width[0], hexmem(mem)),
                  file=self.__logFile)

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        if len(argv) != 1 or not argv[0].isdigit():
            raise gdb.GdbError('trace-calls takes one argument')
        n = int(argv[0], 0)

        # XXX prints are getting annoying
        # XXX Append
        log = self.__logFile = open('trace.out', 'w')

        # Get architecture stuff
        arch = gdb.newest_frame().architecture()
        if arch.name() in ARCH_ADDRS:
            addrDecode = ARCH_ADDRS[arch.name()]
        else:
            print('warning: unknown architecture; memory trace unavailable')
            addrDecode = lambda inst, regs: (None, None)
        inf = self.__inferior = gdb.selected_inferior()

        # Get initial state
        frameName = gdb.newest_frame().name()
        print('FUNC %s' % frameName, file=log)
        regs = self.__get_regs()
        regNames = self.__regNames
        print('REGS %s' % ' '.join('%s=%#x' % (name, val)
                                   for name, val in zip(regNames, regs)),
              file=log)

        # XXX Handle Ctrl-C
        for x in range(n):
            pc = int(gdb.parse_and_eval('$pc'))
            inst = arch.disassemble(pc, count=1)[0]

            # Memory
            instByets = inf.read_memory(pc, inst['length'])
            raddr, waddr = addrDecode(instByets, regs)
            self.__log_mem('PRER', raddr)
            self.__log_mem('PREW', waddr)

            print('STEP %s' % inst['asm'], file=log)
            log.flush()

            disp = gdb.execute('stepi', to_string=True)
            if '\nBreakpoint' in disp:
                # Hit a break point (XXX is there a better way to detect this?)
                print(disp, end='')
                print('tracei stopped after %d steps' % x)
                break

            # Record written memory
            self.__log_mem('POST', waddr)

            # Record changed registers
            nregs = self.__get_regs()
            if nregs != regs:
                print('REGS %s' % ' '.join('%s=%#x' % (regNames[i], nregs[i])
                                           for i in range(len(nregs))
                                           if regs[i] != nregs[i]),
                      file=log)
                regs = nregs

            # Record function change
            nframeName = gdb.newest_frame().name()
            if nframeName != frameName:
                print('FUNC %s' % nframeName, file=log)
                frameName = nframeName

        self.__logFile.close()
        self.__logFile = None

TraceiCmd()
