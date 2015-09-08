fn sigma<F>(a: i32, b: i32, f: F) -> i32
    where F: Fn(i32) -> i32
{
    let mut sum = 0;
    for i in a..(b+1) { sum += f(i) }
    sum
}

macro_rules! test {
    ($exp:expr, $expect:expr) => ({
        let result = $exp;
        assert_eq!(result, $expect);
        println!("{:30} = {}", stringify!($exp), result);
    })
}

fn main() {
    test!(sigma(0, 100, |x| x), 5050);
    test!(sigma(1, 10, |x| x*x), 385);
    test!(sigma(3, 1, |x| x*x), 0);
    test!(sigma(3, 3, |x| x*x*x), 27);
    test!(sigma(-10, -1, |x| x*x), 385);
}
