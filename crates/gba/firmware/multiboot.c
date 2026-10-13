/* Original executable replacement firmware, authored from GBATEK's public
 * multiboot protocol. No retail firmware or emulator implementation is used.
 * Keeping the transfer on the guest CPU makes network waits, IRQs and snapshots
 * use the same scheduler as cartridge-written serial code.
 */
typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;
#define REG16(a) (*(volatile u16 *)(a))
#define REG32(a) (*(volatile u32 *)(a))
#define CNT REG16(0x04000128)
#define SEND REG16(0x0400012a)
#define DATA REG32(0x04000120)

static void delay(u32 count) {
    for (volatile u32 i = 0; i < count; ++i) __asm__ volatile("nop");
}
static u32 crc(u32 value, u32 polynomial) {
    for (u32 bit = 0; bit < 32; ++bit)
        value = (value >> 1) ^ ((value & 1) ? polynomial : 0);
    return value;
}
static u32 host_word(u32 word, u32 mode) {
    if (mode == 1) SEND = word;
    else DATA = word;
    CNT = mode == 1 ? 0x2083 : mode == 2 ? 0x1083 : 0x1081;
    u32 budget = 2000000;
    while ((CNT & 0x80) && --budget) {}
    if (!budget) return 0xffff;
    u32 reply = REG16(0x04000122);
    delay(128); /* More than the required 36 microsecond settling interval. */
    return reply;
}

__attribute__((section(".host")))
u32 multiboot_host(const u8 *p, u32 mode) {
    if (mode > 2 || p[0x1e] != 2) return 1; /* Two-endpoint cable. */
    const u32 *source = *(const u32 * const *)(p + 0x20);
    const u32 *end = *(const u32 * const *)(p + 0x24);
    u32 length = (u32)end - (u32)source;
    if (((u32)source & 3) || length < 0x100 || length > 0x3ff40 || (length & 15)) return 1;
    u32 normal = mode != 1;
    u32 checksum = normal ? 0xc387 : 0xfff8;
    u32 polynomial = normal ? 0xc37b : 0xa517;
    u32 key = normal ? 0x43202f2f : 0x6465646f;
    u32 seed = (u32)p[0x1c] | (u32)p[0x19] << 8 | (u32)p[0x1a] << 16 | (u32)p[0x1b] << 24;
    u32 handshake = 0x11 + p[0x19] + p[0x1a] + p[0x1b];
    if (normal) handshake = p[0x14];
    delay(65536);
    u32 reply = host_word((length >> 2) - 4, mode);
    if ((reply & 0xff00) != 0x7300) return 1;
    u32 finish = (handshake & 255) | (reply & 255) << 8 | 0xffff0000;
    u32 offset = 0xc0;
    while (source != end) {
        u32 plain = *source++;
        checksum = crc(checksum ^ plain, polynomial);
        seed = seed * 0x6f646573 + 1;
        u32 encoded = plain ^ (0u - 0x02000000 - offset) ^ seed ^ key;
        host_word(encoded, mode);
        if (!normal) host_word(encoded >> 16, mode);
        offset += 4;
    }
    checksum = crc(checksum ^ finish, polynomial);
    u32 retries = 1000;
    do { reply = host_word(0x65, mode); } while (reply != 0x75 && --retries);
    if (!retries || host_word(0x66, mode) != 0x75) return 1;
    return host_word(checksum, mode) != checksum;
}

static u16 slave_word(u16 word) {
    SEND = word;
    while (!(CNT & 0x80)) {}
    while (CNT & 0x80) {}
    return (u16)DATA;
}

__attribute__((section(".client"), noreturn))
void multiboot_client(void) {
    REG16(0x04000208) = 0;
    REG16(0x04000134) = 0;
    CNT = 0x2003;
    for (;;) {
        u16 command;
        do { command = slave_word(0x7202); } while (command != 0x6102);
        volatile u16 *header = (volatile u16 *)0x02000000;
        for (u32 i = 0; i < 96; ++i) header[i] = slave_word(((96 - i) << 8) | 2);
        if (slave_word(2) != 0x6200) continue;
        if (slave_word(0x7202) != 0x6202) continue;
        do { command = slave_word(0x7202); } while ((command & 0xff00) != 0x6300);
        u32 palette = command & 255;
        do { command = slave_word(0x735a); } while ((command & 0xff00) == 0x6300);
        if ((command & 0xff00) != 0x6400) continue;
        u32 finish = (command & 255) | 0xffffa700;
        u32 length = ((u32)slave_word(0x73a7) + 0x34) << 2;
        if (length < 0x1c0 || length > 0x40000 || (length & 15)) continue;
        u32 seed = palette | 0xffff5a00;
        u32 checksum = 0xfff8;
        for (u32 offset = 0xc0; offset < length; offset += 4) {
            u32 encoded = slave_word(offset & 0xffff);
            encoded |= (u32)slave_word((offset + 2) & 0xffff) << 16;
            seed = seed * 0x6f646573 + 1;
            u32 plain = encoded ^ (0u - 0x02000000 - offset) ^ seed ^ 0x6465646f;
            REG32(0x02000000 + offset) = plain;
            checksum = crc(checksum ^ plain, 0xa517);
        }
        checksum = crc(checksum ^ finish, 0xa517);
        do { command = slave_word(0x75); } while (command == 0x65);
        if (command != 0x66 || slave_word(checksum) != checksum) continue;
        *(volatile u8 *)0x020000c4 = 3; /* Multiplay boot mode. */
        *(volatile u8 *)0x020000c5 = 1; /* First cable child. */
        *(volatile u8 *)0x03007ffa = 1;
        __asm__ volatile("bx %0" : : "r"(0x020000c0) : "memory");
        __builtin_unreachable();
    }
}
