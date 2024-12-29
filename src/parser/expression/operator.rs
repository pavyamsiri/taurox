#[derive(Debug, Clone, Copy)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

impl PrefixOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 2. Unary operators
            PrefixOperator::Bang | PrefixOperator::Minus => 15,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InfixOperator {
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

impl InfixOperator {
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

#[derive(Debug, Clone, Copy)]
pub enum InfixShortCircuitOperator {
    And,
    Or,
}

impl InfixShortCircuitOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 7. Logical AND operator
            Self::And => (5, 6),
            // 8. Logical OR operator
            Self::Or => (3, 4),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum InfixAssignmentOperator {
    Assign,
}

impl InfixAssignmentOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 9. Assignment operator
            Self::Assign => (2, 1),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PostfixOperator {
    Call,
    Access,
}

impl PostfixOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 0. Call operator
            Self::Access => 19,
            // 1. Call operator
            Self::Call => 17,
        }
    }
}
