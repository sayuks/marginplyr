// Standalone arm64 measurement functions; no Quarto/Deno binary is changed.
.text
.p2align 6
.globl _reset_slots
_reset_slots:
    str xzr, [x0]
    str xzr, [x1]
    ret

// Only selftest uses arbitrary values; trial resets always write two zeros.
.globl _seed_slots
_seed_slots:
    str x2, [x0]
    str x3, [x1]
    ret

.p2align 6
.globl _publish_slots
_publish_slots:
    mov x9, #1
    stur x9, [x0]
    dmb ish
    stur x9, [x1]
    ret

// X0/X1 are independent slot addresses. X2 points to reader-owned output.
// The selected reader path ends at the pre-call pointer observation.
.macro READER name, acquire_barrier
.p2align 6
.globl \name
\name:
    ldr x8, [x1]
    mov x6, x8
    cbnz x8, 1f
    b 2f
1:
    mov x8, x0
.if \acquire_barrier
    dmb ish
.endif
    ldr x8, [x8]
    dmb ish
2:
    stp x6, x8, [x2]
    ret
.endm

READER _read_original, 0
READER _read_barrier, 1

.p2align 4
.globl _delay_before_probe
_delay_before_probe:
    cbz x0, 2f
1:
    nop
    subs x0, x0, #1
    b.ne 1b
2:
    ret
