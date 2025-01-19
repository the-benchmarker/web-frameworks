import "total5";

ROUTE("GET /", function ($) {
  $.text("");
});

ROUTE("GET /user/{id}/", function ($) {
  $.text($.params.id);
});

ROUTE("POST /user/", function ($) {
  $.text("");
});

export default Total
