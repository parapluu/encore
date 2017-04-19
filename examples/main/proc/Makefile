all: StackDriver QueueDriver ListDriver

run: all
	@echo "Running StackDriver..."
	@./StackDriver
	@echo "Running QueueDriver..."
	@./QueueDriver
	@echo "Running ListDriver..."
	@./ListDriver

StackDriver: StackDriver.enc Stack.enc
	encorec $<

QueueDriver: QueueDriver.enc Queue.enc
	encorec $<

ListDriver: ListDriver.enc List.enc
	encorec $<

clean:
	rm -rf StackDriver QueueDriver ListDriver