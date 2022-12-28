mod enums;
use crate::parser::enums::*;

struct Root {
    assignments: Vec<(String, Expression)>,
    starting_expression: Expression,
}
