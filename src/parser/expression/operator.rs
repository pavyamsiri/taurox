#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl UnaryOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 2. Unary operators
            UnaryOperator::Bang | UnaryOperator::Minus => 15,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Multiply,
    Divide,
    Add,
    Subtract,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    EqualEqual,
    BangEqual,
}

impl BinaryOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 3. Multiplicative operators
            Self::Multiply | Self::Divide => (13, 14),
            // 4. Additive operators
            Self::Add | Self::Subtract => (11, 12),
            // 5. Comparison operators
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => {
                (9, 10)
            }
            // 6. Equality operators
            Self::EqualEqual | Self::BangEqual => (7, 9),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryShortCircuitOperator {
    And,
    Or,
}

impl BinaryShortCircuitOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 7. Logical AND operator
            Self::And => (5, 6),
            // 8. Logical OR operator
            Self::Or => (3, 4),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryAssignmentOperator {
    Assign,
}

impl BinaryAssignmentOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 9. Assignment operator
            Self::Assign => (2, 1),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PostfixOperator {
    Call,
}

impl PostfixOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 1. Call operator
            Self::Call => 17,
        }
    }
}
