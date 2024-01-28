module.exports = grammar({
  name: "inferd",

  extras: ($) => [/\s/, token(seq("//", /.*/))],
  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) => choice($.func_def, seq($._expr, ";"), $.assign),
    assign: ($) =>
      seq("let", $.names, optional(seq(":", $._expr)), "=", $._expr, ";"),
    block: ($) =>
      seq(
        "{",
        field("body", repeat($._statement)),
        field("result", optional($._expr)),
        "}",
      ),

    func_def: ($) =>
      prec(
        5,
        seq(
          "fn",
          field("name", $.identifier),
          $.func_proto,
          field("body", choice(";", seq("=", $._expr))),
        ),
      ),
    func_proto: ($) =>
      prec.left(
        seq(
          "(",
          field("params", list($._arg_proto)),
          ")",
          field("return_type", optional($._expr)),
        ),
      ),

    _arg_proto: ($) => choice($.binding_type, $._expr),

    binding_type: ($) => seq($.names, ":", $._expr),
    names: ($) => prec(2, choice($.identifier, seq("(", list($.names), ")"))),

    _expr: ($) =>
      choice($.identifier, $.call_expr, $.type_expr, $.block, $.closure_expr),
    call_expr: ($) => prec(2, seq($._expr, $.tupple)),
    type_expr: ($) =>
      prec(
        1,
        choice(
          $.tupple,
          seq("&", $._expr),
          seq("?", $._expr),
          seq("fn", $.func_proto),
        ),
      ),
    closure_expr: ($) => seq("fn", $.func_proto, "=", field("body", $._expr)),
    tupple: ($) => seq("(", list($._expr), ")"),

    identifier: (_) => /[_\p{XID_Start}][_\p{XID_Continue}]*/,
    // identifier: ($) => /[a-z]+/,
    number: ($) => /\d+/,
  },
});

// fn ((hello,

// Optional trailing comma
function list(e) {
  return seq(repeat(seq(e, ",")), optional(e));
}
