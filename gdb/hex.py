# Hex dump gdb command
#
# Written by Austin Clements

import gdb

class Hex(gdb.Command):
    """Hexdump memory: hex/LEN ADDRESS

LEN is optional and can be given in decimal, hexadecimal, or octal.
If omitted, LEN defaults to the size of the object being displayed if
that's meaningful, or 128 bytes.

ADDRESS is a number, pointer, or object.  If it is a number or
pointer, the memory at this address will be displayed.  If it is an
object, the memory at that object's address will be displayed.  This
is similar to the 'x' command.

This command sets the convenience variable $_ to the address just
beyond the last printed address.

This command can be invoked with no arguments (just "hex") and will
display the memory pointed to by $_.  This command can also be
repeated (by simply pressing enter at the gdb prompt) and will behave
as if invoked with no arguments on repeated invocations, displaying
memory starting with where the last display left off."""

    def __init__(self):
        super(Hex, self).__init__("hex", gdb.COMMAND_DATA,
                                  gdb.COMPLETE_EXPRESSION)
        self.__last_history = None
        self.__last_len = None

    def __is_repeat(self):
        # GDB doesn't tell us if a command was a repeat, so the only
        # way I've found to check this is to look at the command
        # history and see that it hasn't changed.
        history = gdb.execute('show commands', to_string=True)
        repeat = (self.__last_history == history)
        self.__last_history = history
        return repeat

    def invoke(self, arg, from_tty):
        length = None
        if self.__is_repeat():
            # For a repeat, GDB will pass the same arguments as
            # before, but we want to continue from where we left off.
            # This is equivalent to running with no arguments.
            arg = ''
            length = self.__last_len

        # Parse optional length argument
        if arg.startswith('/'):
            prefix, arg = arg[1:].split(None, 1)
            try:
                length = int(prefix, 0)
            except ValueError:
                raise gdb.GdbError('Bad length %s' % prefix)

        # Parse address argument
        if len(arg.strip()):
            try:
                addrval = gdb.parse_and_eval(arg)
            except gdb.error as e:
                raise gdb.GdbError(str(e))
        else:
            addrval = gdb.parse_and_eval('$_')
            if addrval.type.code == gdb.TYPE_CODE_VOID:
                # There is no previous address yet
                raise gdb.GdbError(
                    'Argument required (starting display address)')
        if addrval.type.code not in (gdb.TYPE_CODE_PTR, gdb.TYPE_CODE_INT) and \
           addrval.address is not None:
            # Some object type.  Take a pointer to it.
            addrval = addrval.address

        # Compute length if not provided
        if length is None:
            if addrval.type.strip_typedefs().code == gdb.TYPE_CODE_PTR and \
               addrval.type.strip_typedefs().target().strip_typedefs().code \
               not in (gdb.TYPE_CODE_FUNC, gdb.TYPE_CODE_VOID):
                # Use the object's length
                length = addrval.type.target().sizeof
            else:
                # Use a default length
                length = 16 * 8
        self.__last_len = length

        # Retrieve memory
        try:
            addr = long(addrval)
        except gdb.error as e:
            # Happens when things are optimized out, don't have an
            # address, etc.
            raise gdb.GdbError(str(e))
        error = None
        try:
            mem = gdb.selected_inferior().read_memory(addr, length)
        except gdb.MemoryError as e:
            if not e.args[0].startswith('Cannot access memory at address '):
                raise
            length = int(e.args[0].rsplit(None, 1)[-1], 0) - addr
            mem = gdb.selected_inferior().read_memory(addr, length)
            error = str(e)

        # Dump
        try:
            self.__hexdump(mem, addr)
            if error is not None:
                print(error)
        except KeyboardInterrupt:
            # "q" in the pager raises this
            return

        # Set $_ to last address
        gdb.parse_and_eval('$_ = (void*)%#x' % (addr + length))

    def __hexdump(self, mem, addr):
        length = len(mem)
        awidth = len('%x' % (addr + length))
        for laddr in xrange(addr, addr + length, 16):
            line = '%0*x  ' % (awidth, laddr)
            text = ''
            for subaddr in range(laddr, min(addr + length, laddr + 16)):
                if subaddr - laddr == 8:
                    line += ' '
                char = mem[subaddr - addr]
                line += '%02x ' % ord(char)
                if 0x20 <= ord(char) < 0x7f:
                    text += char
                else:
                    text += '.'
            line = line.ljust(awidth + 2 + 3 * 16 + 2) + '|' + text + '|'

            print(line)

Hex()
