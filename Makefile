compile:
	./rebar compile
    
clean_generate:
	rm -rf rel/chatapp
    
generate:
	cd rel && ../rebar generate

console:
	./rel/chatapp/bin/chatapp console

quick-console:
	erl -config rel/files/sys.config -pa  apps/*/ebin -pa vendor/*/ebin -pa -boot start_sasl -eval "reloader:start(),application:start(chatapp)"

all: compile clean_generate generate