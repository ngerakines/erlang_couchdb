all: code

code: clean
	erlc +debug_info -o ./ebin src/erlang_couchdb.erl
	erlc +debug_info -o ./ebin src/couchdb.erl

clean:
	rm -rfv ebin/*.beam erl_crash.dump

dist-src: clean
	tar zcvf erlang_couchdb-0.2.3.tgz Makefile src/

test: all
	prove -v t/*.t

cover: all
	COVER=1 prove t/*.t
	erl -noshell -eval 'etap_report:create()' -s init stop
