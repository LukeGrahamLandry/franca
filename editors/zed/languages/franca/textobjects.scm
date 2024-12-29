; functions
(function_signature_item) @function.around

(function_item
    body: (_
        "{"
        (_)* @function.inside
        "}" )) @function.around

; comments

(line_comment)+ @comment.around

(block_comment) @comment.around