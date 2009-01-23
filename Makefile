all: code

code: clean
	erlc -o ebin src/erlang_couchdb.erl

clean:
	rm -rfv ebin/*.beam erl_crash.dump

dist-src: clean
	tar zcvf erlang_couchdb-0.2.3.tgz Makefile src/
