FROM thevlang/vlang

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN v . -o server

CMD {{command}}