WORKER_AMOUNT = 256
TIME = /usr/bin/time -f 'wall-time: %E\nuser-time: %U \nsystem-time: %S\nload: %P' 
ENC=big
ERLANG=Erlang-Hipe/big.beam
SCALA=Scala/big.class
THREADS=--ponythreads 4

all:
	@cd Erlang-Hipe; which erlc && make; true
	@cd Scala; which scalac && make; true
	@encorec -c big.enc

encore: big.enc
	encorec -c big.enc

bench:	
ifneq ("$(wildcard $(ENC))","")
	@echo '--==Encore==--'
	@echo ' '
	@$(TIME)  ./big $(THREADS) $(WORKER_AMOUNT)
	@echo ' '
	@$(TIME)  ./big $(THREADS) $(WORKER_AMOUNT)
	@echo ' '
	@$(TIME)  ./big $(THREADS) $(WORKER_AMOUNT)
	@echo ''
endif

ifneq ("$(wildcard $(ERLANG))","")
	@echo '--==Erlang==--'
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
	@echo ' '
	@cd Erlang-Hipe; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
endif

ifneq ("$(wildcard $(SCALA))","")
	@echo '--==Scala==--'
	@echo ' '
	@cd Scala; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
	@echo ' '
	@cd Scala; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
	@echo ' '
	@cd Scala; $(TIME)  make WORKER_AMOUNT=$(WORKER_AMOUNT) run
endif
