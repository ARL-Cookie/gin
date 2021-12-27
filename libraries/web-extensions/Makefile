PKG_MODULES := gobject-2.0 webkit2gtk-web-extension-4.0

CFLAGS += ${shell pkg-config ${PKG_MODULES} --cflags} -fPIC
LDLIBS += ${shell pkg-config ${PKG_MODULES} --libs}

.PHONY: all install clean
all: libnyxt.so

libnyxt.so: nyxt.o globals.o extevent.o browser.o tabs.o management.o runtime.o extension.o storage.o alarms.o bookmarks.o browser_action.o commands.o history.o notifications.o permissions.o web_navigation.o web_request.o
	${LD} ${LDFLAGS} -fPIC -shared -o $@ $^ ${LDLIBS}

install: libnyxt.so
	${INSTALL} $^

clean:
	${RM} libnyxt.so nyxt.o globals.o extevent.o browser.o tabs.o management.o runtime.o extension.o storage.o alarms.o bookmarks.o browser_action.o commands.o history.o notifications.o permissions.o web_navigation.o web_request.o