module.exports = grammar({
  name: "inferd",

  extras: ($) => [
    /\s/,
    token(seq("//", /.*/, "\n")),
    // TODO
    // token(seq("/*", /.*/, "*/")),
  ],
  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice($.func_def, seq(choice($.expr, $.declare, $.assign), ";"), ";"),
    declare: ($) =>
      seq(
        choice("let", "var", "const"),
        $.binding_type,
        optional(seq("=", $.expr)),
      ),
    assign: ($) => seq($.names, "=", $.expr),
    block: ($) =>
      seq(
        "{",
        field("body", repeat($._statement)),
        field("result", optional($.expr)),
        "}",
      ),

    func_def: ($) =>
      prec(
        5,
        seq(
          optional(field("annotation", repeat($.annotation))),
          "fn",
          field("name", $.identifier),
          field("proto", $.func_proto),
          choice(
            ";",
            seq("=", field("body", $.expr), ";"),
            seq("=", field("body", $.block)),
          ),
        ),
      ),
    annotation: ($) => seq("@", $.identifier, optional($.tuple)),
    func_proto: ($) =>
      prec.left(
        seq(
          "(",
          field("params", list($._arg_proto)),
          ")",
          field("return_type", optional($.expr)),
        ),
      ),

    _arg_proto: ($) => choice($.binding_type, $.expr),

    binding_type: ($) => seq($.names, optional(seq(":", $.expr))),
    names: ($) => prec(2, choice($.identifier, seq("(", list($.names), ")"))),

    expr: ($) =>
      prec.left(
        seq(
          choice(
            $.identifier,
            $.call_expr,
            $.type_expr,
            $.block,
            $.closure_expr,
            $.number,
            $.anon_arg,
          ),
          optional($.suffix_macro),
        ),
      ),
    suffix_macro: ($) => seq("!", $.identifier),
    anon_arg: ($) => prec(5, seq("$", $.number)),
    call_expr: ($) => prec(2, seq($.expr, $.tuple)),
    type_expr: ($) =>
      prec(
        1,
        choice(
          $.tuple,
          seq("&", $.expr),
          seq("?", $.expr),
          seq("fn", $.func_proto),
        ),
      ),
    closure_expr: ($) =>
      prec(
        // high prec so if there's a body it counts as this not a type then a random equals sign i guess?
        3,
        choice(
          seq("fn", field("proto", $.func_proto), "=", field("body", $.expr)),
          seq("fn", "=", field("body", $.expr)),
        ),
      ),
    tuple: ($) => seq("(", list($.expr), ")"),

    identifier: (_) => /[_\p{XID_Start}][_\p{XID_Continue}]*/,
    // identifier: ($) => /[a-z]+/,
    number: ($) => /\d+/,
  },
});

// Optional trailing comma
function list(e) {
  return seq(repeat(seq(e, ",")), optional(e));
}
