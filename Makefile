all: code

code: clean
	erlc src/erlang_couchdb.erl

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump erlang_couchdb/ *.deb

package-debian: code
	mkdir -p erlang_couchdb/usr/lib/erlang/lib/erlang_couchdb-0.1/ebin/ && cp erlang_couchdb.beam erlang_couchdb/usr/lib/erlang/lib/erlang_couchdb-0.1/ebin/erlang_couchdb.beam
	mkdir -p erlang_couchdb/DEBIAN/ && cp control erlang_couchdb/DEBIAN/control
	dpkg -b erlang_couchdb erlang_couchdb.deb

install-debian: package-debian
	dpkg -i erlang_couchdb.deb
