// TODO: this should say something about undeclared var but it says 
//       `^ unknown type for left of ND_COND (left is ND_STMT)` 
//        because it makes a silly node because of :LabelAsInfix
//        (it was even worse before 11ac291. i was setting `node.ty = c.ty_int;` so this wasn't an error)
void b() { 1 ? a : 0; }
