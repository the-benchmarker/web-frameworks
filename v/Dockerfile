FROM thevlang/vlang

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN v . -o server

WORKDIR /app

CMD {{command}}