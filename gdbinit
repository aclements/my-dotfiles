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
