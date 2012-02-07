# include debian/version.mk
ERLROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR := $(ERLROOT)/lib/udpts-$(VERSION)
DESTROOT := $(CURDIR)/debian/udpts
OS = $(shell uname -s)
ifeq ("$(OS)", "Linux")
LIBFLAGS := -shared -fpic -I /usr/lib/erlang/erts-5.9/include -I /usr/local/lib/erlang/erts-5.9/include
else
LIBFLAGS := -arch x86_64 -pipe -bundle -undefined dynamic_lookup -I /usr/local/lib/erlang/erts-5.9/include
endif  

all: compile
  
ebin/udpts_drv.so: src/udpts.c
	gcc -o ebin/udpts_drv.so src/udpts.c $(LIBFLAGS)

compile: ebin/udpts_drv.so
	erl -make
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

priv/udpts.conf:
	[ -f priv/udpts.conf ] || cp priv/udpts.conf.sample priv/udpts.conf

run: compile priv/udpts.conf
	ERL_LIBS=/usr/local/lib:.. erl +A 4 +K true -pa ebin -boot start_sasl -s udpts -sname udpts

start: compile
	ERL_LIBS=/usr/local/lib:.. erl +A 4 +K true -pa ebin -boot start_sasl -sasl sasl_error_logger '{file, "error.log"}' -noinput -detached -s udpts -sname udpts
	
test:
	@erl -pa ebin -s udpts test -noshell -noinput -s init stop



archive:
	git archive --prefix=udpts/ master | gzip -9 > udpts.tar.gz

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/



.PHONY: doc debian compile
