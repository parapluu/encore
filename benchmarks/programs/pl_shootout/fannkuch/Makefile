all: scala erlang encore

encore: 
	encorec -c fannkuch_redux.enc
	encorec -c fannkuch_foreach.enc

scala: 
	@cd Scala; which scalac && scalac fannkuch_redux.scala; true

erlang:
	@cd Erlang-Hipe; which erlc && erlc +native +"{hipe, [o3]}" fannkuchredux.erl
