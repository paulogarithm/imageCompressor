##
## EPITECH PROJECT, 2024
## Paradigm Pool
## File description:
## Makefile
##

NAME	=	imageCompressor

STACK_BUILD	=	stack build
STACK_PATH	=	stack path

BIN_PATH	:=	$(shell $(STACK_PATH) --local-install-root)

all: $(NAME)

$(NAME):
	$(STACK_BUILD)
	cp $(BIN_PATH)/bin/$(NAME) .

clean:
	stack purge

fclean: clean
	$(RM) $(NAME)

re:	fclean all
