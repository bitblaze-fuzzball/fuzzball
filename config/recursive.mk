# got it from 
# http://uucode.com/blog/2005/08/10/adding-recursive-targets-to-automake/
# sojeong

bytecode-recursive:

ifneq ($(RECURSIVE_TARGETS),bytecode-recursive)
bytecode-recursive:
	$(MAKE) $(AM_MAKEFLAGS) RECURSIVE_TARGETS=bytecode-recursive bytecode-recursive
endif

bytecode: all bytecode-recursive bytecode-am

bytecode-am: bytecode-local

bytecode-local:
