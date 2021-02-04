FROM node:14.15.4-buster

RUN git config --global user.email "elm-program-test-ci@.(none)"
RUN git config --global user.name "elm-program-test-ci"
RUN mkdir /workspace

RUN npm install -g pnpm
COPY ci.sh /

ENTRYPOINT [ "/ci.sh" ]
