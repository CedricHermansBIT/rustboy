//! Redistributable SameBoy replacement boot ROMs (Expat license), not Nintendo
//! firmware. Compiled into the backend so standalone WASM builds can boot.
pub const LICENSE: &str = include_str!("../third_party/sameboy/LICENSE");
pub static DMG: [u8; 256] = decode(include_str!("../third_party/sameboy/dmg_boot.hex"));
pub static CGB: [u8; 2304] = decode(include_str!("../third_party/sameboy/cgb_boot.hex"));

const fn decode<const N: usize>(hex: &str) -> [u8; N] {
    let source = hex.as_bytes();
    let mut bytes = [0; N];
    let mut input = 0;
    let mut nibbles = 0;
    while input < source.len() {
        let digit = match source[input] {
            b'0'..=b'9' => source[input] - b'0',
            b'a'..=b'f' => source[input] - b'a' + 10,
            b' ' | b'\n' | b'\r' | b'\t' => {
                input += 1;
                continue;
            }
            _ => panic!("invalid replacement boot ROM hex"),
        };
        assert!(nibbles < N * 2, "replacement boot ROM too long");
        bytes[nibbles / 2] = (bytes[nibbles / 2] << 4) | digit;
        nibbles += 1;
        input += 1;
    }
    assert!(nibbles == N * 2, "replacement boot ROM truncated");
    bytes
}
