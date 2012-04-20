set history save

python
import sys, os
libstdcxx_path = os.path.expanduser("~/sys/dotfiles/gdb/python")
if os.path.isdir(libstdcxx_path):
  sys.path.insert(0, libstdcxx_path)
  from libstdcxx.v6.printers import register_libstdcxx_printers
  register_libstdcxx_printers (None)
else:
  print "libstdcxx pretty-printers not found"
del libstdcxx_path
end

python
class Resize(gdb.Command):
  def __init__(self):
    super(Resize, self).__init__("resize", gdb.COMMAND_SUPPORT)

  def invoke(self, arg, from_tty):
    import subprocess
    p = subprocess.Popen(["stty", "size"], stdout=subprocess.PIPE)
    size = p.stdout.read().split()
    if not p.wait():
      gdb.execute("set height " + size[0])
      gdb.execute("set width " + size[1])
Resize()
end

define hook-stop
  resize
end
