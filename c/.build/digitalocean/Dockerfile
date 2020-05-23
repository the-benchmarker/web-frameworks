FROM gcc

WORKDIR /opt/web

{{#sources}}
   COPY {{{.}}} {{{.}}}
{{/sources}}

{{#download}}
  RUN {{{.}}}
{{/download}}

{{#build}}
   RUN {{{.}}}
{{/build}}

# Workaround for kore
RUN [ -f /usr/local/bin/kore ] && mv /usr/local/bin/kore kore || true
