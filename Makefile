# include debian/version.mk
ERLROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR := $(ERLROOT)/lib/udpts-$(VERSION)
DESTROOT := $(CURDIR)/debian/udpts


all: compile

compile:
	erl -make
	
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

run: compile
	ERL_LIBS=/usr/local/lib erl -pa ebin -boot start_sasl -s udpts -sname udpts

start: compile
	ERL_LIBS=/usr/local/lib erl -pa ebin -boot start_sasl -noinput -detached -s udpts -sname udpts
	
test:
	@erl -pa ebin -s udpts test -noshell -noinput -s init stop


archive: compile
	erl -pa ebin -hidden -noshell -s udpts archive -s init stop


install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/



.PHONY: doc debian compile
