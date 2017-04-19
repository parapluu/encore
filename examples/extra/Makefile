all: LockDriver

run: all
	@echo "Running LockDriver..."
	@./LockDriver

LockDriver: LockDriver.enc Lock.enc
	encorec $<

clean:
	rm -rf LockDriver