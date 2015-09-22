all: scala erlang encore

encore: chameneos_redux.enc
	encorec -c chameneos_redux.enc

scala: 
	@cd Scala; which scalac && scalac chameneos_redux.scala; true

erlang:
	@cd Erlang-Hipe; which erlc && erlc +native +"{hipe, [o3]}" chameneos_redux.erl
