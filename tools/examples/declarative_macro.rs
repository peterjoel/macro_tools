use macro_tools::repeat_count;

macro_rules! print_n {
    ($n: literal, $msg: literal) => {
        println!("{} - {}", $n, $msg);
    };
}

fn main() {
    repeat_count!(print_n, 'a'..='g', "hello");
}
