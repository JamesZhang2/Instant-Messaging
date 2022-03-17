.PHONY: test check

zip:
	rm -f Instant-Messaging.zip
	zip -r Instant-Messaging.zip . -x@exclude.lst
