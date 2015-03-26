# got it from 
# http://uucode.com/blog/2005/08/10/adding-recursive-targets-to-automake/
# sojeong

# There's apparently proper support for this in automake 1.13 forward,
# but I think it's too soon to depend on that

RECURSIVE_TARGETS += bytecode-recursive nodebug-recursive

bytecode-recursive:

ifneq ($(RECURSIVE_TARGETS),bytecode-recursive)
bytecode-recursive:
	$(MAKE) $(AM_MAKEFLAGS) RECURSIVE_TARGETS=bytecode-recursive bytecode-recursive
endif

bytecode: all bytecode-recursive bytecode-am

bytecode-am: bytecode-local

bytecode-local:

nodebug-recursive:

ifneq ($(RECURSIVE_TARGETS),all-recursive nodebug-recursive)
nodebug-recursive:
	$(MAKE) $(AM_MAKEFLAGS) RECURSIVE_TARGETS="all-recursive nodebug-recursive" nodebug-recursive
endif

nodebug: nodebug-recursive nodebug-am

nodebug-am: nodebug-local

nodebug-local:
