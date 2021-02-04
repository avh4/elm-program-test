FROM node:14.15.4-buster

RUN npm install -g pnpm
RUN mkdir /workspace
COPY ci.sh /

ENTRYPOINT [ "/ci.sh" ]
