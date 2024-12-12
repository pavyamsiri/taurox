use crate::parser::expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    InfixOperator, InfixShortCircuitOperator, PrefixOperator,
};

use super::{
    environment::Environment,
    error::{RuntimeError, RuntimeErrorKind},
    value::LoxValue,
};

pub struct ExpressionEvaluator;

impl ExpressionEvaluator {
    fn evaluate_expression_atom(
        atom: &ExpressionAtom,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        let result = match &atom.kind {
            ExpressionAtomKind::Number(v) => LoxValue::Number(*v),
            ExpressionAtomKind::Bool(v) => LoxValue::Bool(*v),
            ExpressionAtomKind::Nil => LoxValue::Nil,
            ExpressionAtomKind::StringLiteral(ref v) => LoxValue::String(v.clone()),
            ExpressionAtomKind::Identifier(ref name) => environment
                .access(name)
                .ok_or(RuntimeErrorKind::InvalidAccess(name.clone()))?
                .clone(),
        };
        Ok(result)
    }

    fn evaluate_expression_node(
        tree: &Expression,
        node: &ExpressionNodeRef,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = tree
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let line = tree
            .get_line(node)
            .expect("Node ref came from the tree so it must exist.");

        let result = match current_node {
            ExpressionNode::Atom(atom) => Self::evaluate_expression_atom(atom, environment)
                .map_err(|kind| RuntimeError { kind, line })?,
            ExpressionNode::Prefix { operator, rhs } => {
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                Self::evaluate_unary(operator, &rhs).map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionNode::Group { inner } => {
                Self::evaluate_expression_node(tree, inner, environment)?
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                let lhs = Self::evaluate_expression_node(tree, lhs, environment)?;
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                Self::evaluate_binary(operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                let _ = environment
                    .assign(lhs, rhs.clone())
                    .map_err(|_| RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(lhs.clone()),
                        line,
                    })?;
                rhs
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => {
                Self::evaluate_binary_short_circuit(operator, lhs, rhs, tree, environment)?
            }
            ExpressionNode::Call { callee, arguments } => {
                let callee = Self::evaluate_expression_node(tree, callee, environment)?;
                match callee {
                    LoxValue::NativeFunction(fun) => {
                        // Set up scope
                        environment.enter_scope();

                        // Check that the argument list is the same length as the parameter list.
                        if arguments.len() != fun.get_parameters().len() {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::InvalidArgumentCount {
                                    actual: arguments.len(),
                                    expected: fun.get_parameters().len(),
                                },
                                line,
                            });
                        }

                        // Define the arguments in the function scope
                        for (name, argument) in fun.get_parameters().iter().zip(arguments.iter()) {
                            let argument =
                                Self::evaluate_expression_node(tree, argument, environment)?;
                            environment.declare(name, argument);
                        }

                        let result = fun.call(environment)?;

                        environment.exit_scope();
                        result
                    }
                    LoxValue::Function {
                        name: _,
                        parameters,
                        body: _,
                    } => {
                        // Set up scope
                        environment.enter_scope();

                        // Check that the argument list is the same length as the parameter list.
                        if arguments.len() != parameters.len() {
                            return Err(RuntimeError {
                                kind: RuntimeErrorKind::InvalidArgumentCount {
                                    actual: arguments.len(),
                                    expected: parameters.len(),
                                },
                                line,
                            });
                        }

                        // Define the arguments in the function scope
                        for (name, argument) in parameters.iter().zip(arguments.iter()) {
                            let argument =
                                Self::evaluate_expression_node(tree, argument, environment)?;
                            environment.declare(name, argument);
                        }

                        // TODO(pavyamsiri): Refactor to be able to interpret statements here.
                        todo!();
                    }
                    v => {
                        return Err(RuntimeError {
                            kind: RuntimeErrorKind::InvalidCallee(v),
                            line,
                        });
                    }
                }
            }
        };
        Ok(result)
    }

    pub fn evaluate_expression(
        tree: &Expression,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        Self::evaluate_expression_node(tree, &tree.get_root_ref(), environment)
    }

    fn evaluate_unary(
        operator: &PrefixOperator,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        match operator {
            PrefixOperator::Bang => Ok(LoxValue::Bool(rhs.logical_not())),
            PrefixOperator::Minus => rhs.numeric_negate(),
        }
    }

    fn evaluate_binary(
        operator: &InfixOperator,
        lhs: &LoxValue,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        match operator {
            InfixOperator::Add => lhs.add(rhs),
            InfixOperator::Subtract => lhs.subtract(rhs),
            InfixOperator::Multiply => lhs.multiply(rhs),
            InfixOperator::Divide => lhs.divide(rhs),
            InfixOperator::LessThan => lhs.less_than(rhs),
            InfixOperator::LessThanEqual => lhs.less_than_or_equal(rhs),
            InfixOperator::GreaterThan => lhs.greater_than(rhs),
            InfixOperator::GreaterThanEqual => lhs.greater_than_or_equal(rhs),
            InfixOperator::EqualEqual => Ok(LoxValue::Bool(lhs.is_equal(rhs))),
            InfixOperator::BangEqual => Ok(LoxValue::Bool(lhs.is_not_equal(rhs))),
        }
    }

    fn evaluate_binary_short_circuit(
        operator: &InfixShortCircuitOperator,
        lhs: &ExpressionNodeRef,
        rhs: &ExpressionNodeRef,
        tree: &Expression,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        let lhs = { Self::evaluate_expression_node(tree, lhs, environment)? };

        match operator {
            InfixShortCircuitOperator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                    Ok(rhs)
                }
            }
            InfixShortCircuitOperator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                    Ok(rhs)
                }
            }
        }
    }
}
