FROM haskell:9.4.8 as build
RUN mkdir -p /benjbot/build
WORKDIR /benjbot/build

# system lib dependencies
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3-dev build-essential pkg-config libicu-dev --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

COPY . . 

RUN stack build --system-ghc

RUN mv "$(cabal path --local-install-root)/bin" /benjbot/build/bin

FROM haskell:9.4.8-slim as app

# system runtime deps
RUN apt-get update -qq && \
  apt-get install -qq -y libpcre3 libicu63 --fix-missing --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p /benjbot
WORKDIR /benjbot

COPY --from=build /benjibot/build/bin .
# apparently we need the .git folder
COPY .git .git 
# we need fonts for the roll stats
COPY fonts fonts
CMD /benjibot/benjibot-exe
