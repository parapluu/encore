TEST_DIRECTORY=basic modules par

test: $(TEST_DIRECTORY)

$(TEST_DIRECTORY):
	@make -C $@ clean test

clean:
	@$(foreach FOLDER,$(TEST_DIRECTORY), make -C $(FOLDER) clean;)

.PHONY: test clean $(TEST_DIRECTORY)
