all:
	mkdir -p ebin/
	(cd src;$(MAKE))

compile:    ${MODS:%=%.beam}

ct:	compile
	cp ./ebin/*.beam ./test
	mkdir -p ../../Erlang/erlandom/pshb/webhook/htdocs/erlang_couchdbtest
	/usr/local/lib/erlang/lib//common_test-1.4.5/priv/bin/run_test -dir . -logdir ../../Erlang/erlandom/pshb/webhook/htdocs/erlang_couchdbtest -cover ./config/couchdb.coverspec

test: all
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	rm -rf erl_crash.dump *.beam *.hrl cover

dist-src: clean
	tar zcvf erlang_couchdb-0.2.3.tgz Makefile src/

cover: all
	COVER=1 prove t/*.t
	erl -detached -noshell -eval 'etap_report:create()' -s init stop
