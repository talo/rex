#[macro_export]
macro_rules! trace {
    ($c:expr) => {
    };
    ($c:expr, $($arg:tt)*) => {{
        if ($c.trace_eval) {
            print!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! traceln {
    ($c:expr) => {
        if ($c.trace_eval) {
            println!();
        }
    };
    ($c:expr, $($arg:tt)*) => {{
        if ($c.trace_eval) {
            println!($($arg)*);
        }
    }};
}
