DESTROOT := $(CURDIR)/debian/udpts
OS = $(shell uname -s)

all: compile
  
compile: 
	./rebar compile
	
clean:
	./rebar clean
	rm -fv erl_crash.dump

priv/udpts.conf:
	[ -f priv/udpts.conf ] || cp priv/udpts.conf.sample priv/udpts.conf

run: compile priv/udpts.conf
	ERL_LIBS=/usr/local/lib:.. erl +A 4 +K true -pa ebin -boot start_sasl -s udpts -sname udpts

start: compile
	ERL_LIBS=/usr/local/lib:.. erl +A 4 +K true -pa ebin -boot start_sasl -sasl sasl_error_logger '{file, "error.log"}' -noinput -detached -s udpts
	
test:
	@erl -pa ebin -s udpts test -noshell -noinput -s init stop



archive:
	git archive --prefix=udpts/ master | gzip -9 > udpts.tar.gz

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/



.PHONY: doc debian compile
