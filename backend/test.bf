
fn greet() {
    let hello: str<10> = "Hello,    ";
    let world: str<10> = "World!    ";
    strOut(hello);
    strOut(world);

    let name: str<10> = "Alice     ";
    strOut("Hello     ");
    strOut(name);
    strOut("!\n        ");
}

fn strOut(s: str<10>) {
    std::out(s[0u8]);
    std::out(s[1u8]);
    std::out(s[2u8]);
    std::out(s[3u8]);
    std::out(s[4u8]);
    std::out(s[5u8]);
    std::out(s[6u8]);
    std::out(s[7u8]);
    std::out(s[8u8]);
    std::out(s[9u8]);
    std::out('\n');
}

fn main() {
    let integer: u8 = 42u8;

    std::out(integer);

    greet();
}