/// Homework 1 - Exercise 1
fn sigma<F>(a: i64, b: i64, f: F) -> i64
    where F: Fn(i64) -> i64
{
    let mut sum = 0;
    for i in a..(b+1) { sum += f(i) }
    sum
}

/// Homework 1 - Exercise 2
fn eval(formula: Formula) -> bool {
    fn value(expr: Expr) -> i64 {
        match expr {
            Expr::Num(value) => value,
            Expr::Plus(left, right) => value(*left) + value(*right),
            Expr::Minus(left, right) => value(*left) - value(*right)
        }
    }

    match formula {
        Formula::True => true,
        Formula::False => false,
        Formula::Not(formula) => !eval(*formula),
        Formula::AndAlso(left, right) => eval(*left) && eval(*right),
        Formula::OrElse(left, right) => eval(*left) || eval(*right),
        Formula::Imply(left, right) => !eval(*left) || eval(*right),
        Formula::Less(v0, v1) => value(*v0) < value(*v1)
    }
}

enum Formula {
    True,
    False,
    Not(Box<Formula>),
    AndAlso(Box<Formula>, Box<Formula>),
    OrElse(Box<Formula>, Box<Formula>),
    Imply(Box<Formula>, Box<Formula>),
    Less(Box<Expr>, Box<Expr>)
}

enum Expr {
    Num(i64),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>)
}

macro_rules! test {
    ($expect:expr, $exp:expr) => ({
        let result = $exp;
        assert_eq!(result, $expect);
        println!("{:30} = {}", stringify!($exp), result);
    })
}

fn main() {
    println!("Homework 1 - Exercise 1");
    test!(5050, sigma(0, 100, |x| x));
    test!(385, sigma(1, 10, |x| x*x));
    test!(0, sigma(3, 1, |x| x*x));
    test!(27, sigma(3, 3, |x| x*x*x));
    test!(385, sigma(-10, -1, |x| x*x));
    println!("");

    {
        use Formula::*;
        println!("Homework 1 - Exercise 2");
        test!(true, eval(True));
        test!(false, eval(False));
        test!(false, eval(Not(Box::new(True))));
        /*
        test!(true, eval (Not False));
        test!(true, eval (AndAlso (True, True)));
        test!(false, eval (AndAlso (True, False)));
        test!(false, eval (AndAlso (False, True)));
        test!(false, eval (AndAlso (False, False)));
        test!(true, eval (OrElse (True, True)));
        test!(true, eval (OrElse (True, False)));
        test!(true, eval (OrElse (False, True)));
        test!(false, eval (OrElse (False, False)));
        test!(false, eval (Imply (True, False)));
        test!(true, eval (Imply (True, True)));
        test!(true, eval (Imply (False, True)));
        test!(true, eval (Imply (False, False)));
        test!(true, eval (Less (Num 3, Num 5)));
        test!(false, eval (Less (Num 3, Num 3)));
        test!(false, eval (Less (Num 3, Num 1)));
        test!(false, eval (Less (Plus (Num 3, Num 4), Minus (Num 5, Num 1))));
        test!(true, eval (Less (Plus (Num 10, Num 12), Minus (Num 10, Num (-13)))));
        */
        println!("");
    }
}
