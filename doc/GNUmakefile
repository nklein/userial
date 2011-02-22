
NAME := userial

NWS :=	_head.nw \
	  0_intro.nw \
	  1_buffer.nw \
	  2_serialize.nw \
	  3_protocol.nw \
	_tail.nw

%.pdf:	%.tex
	pdflatex $<
	pdflatex $<
	pdflatex $<

all:	$(NAME).pdf

$(NAME).tex: $(NWS)
	noweave -latex -index -delay $^ | cpif $@

clean: tidy
	rm -f $(NAME).pdf

tidy:
	rm -f $(NAME).aux
	rm -f $(NAME).idx
	rm -f $(NAME).log
	rm -f $(NAME).out
	rm -f $(NAME).tex
	rm -f $(NAME).toc

.PHONY: all
.PHONY: clean
.PHONY: tidy