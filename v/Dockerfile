FROM thevlang/vlang

WORKDIR /app

{{#files}}
  COPY '{{source}}' '{{target}}'
{{/files}}

RUN v .

WORKDIR /app

CMD {{command}}