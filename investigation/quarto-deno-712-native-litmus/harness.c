// Investigation-only harness. Shared measured slots are accessed only in probe.S.
#include <inttypes.h>
#include <pthread.h>
#include <signal.h>
#include <stdalign.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysctl.h>
#include <time.h>
#include <unistd.h>

#if !defined(__aarch64__)
#error This experiment requires a native AArch64 build.
#endif

typedef struct { uint64_t guard, callback; } observation;
typedef struct { alignas(128) _Atomic uint64_t value; } epoch;
typedef void (*reader_fn)(void *, void *, observation *);
extern void reset_slots(void *, void *);
extern void seed_slots(void *, void *, uint64_t, uint64_t);
extern void publish_slots(void *, void *);
extern void read_original(void *, void *, observation *);
extern void read_barrier(void *, void *, observation *);
extern void delay_before_probe(uint64_t);

static alignas(128) unsigned char slots[384];
static epoch start_epoch, done_writer, done_reader;
static alignas(128) observation result;
static void *const init_slot = slots + 120;
static void *const alloc_slot = slots + 136;
static uint64_t iterations;
static reader_fn reader;

// The watchdog bounds stalled epoch waits as well as the finite measurement loop.
static void timeout_handler(int sig) {
    (void)sig;
    const char message[] = "native litmus exceeded its 30-second watchdog\n";
    (void)write(STDERR_FILENO, message, sizeof(message) - 1);
    _exit(124);
}

static void wait_epoch(epoch *e, uint64_t expected) {
    while (atomic_load_explicit(&e->value, memory_order_acquire) != expected) {
        __asm__ volatile("yield");
    }
}

// Fixed per-role seeds and bounded delays are identical in every trial.
static uint32_t next_delay(uint32_t *state) {
    *state ^= *state << 13;
    *state ^= *state >> 17;
    *state ^= *state << 5;
    return *state & 63;
}

static void *writer_worker(void *unused) {
    (void)unused;
    uint32_t state = UINT32_C(0x712a713b);
    for (uint64_t i = 1; i <= iterations; ++i) {
        wait_epoch(&start_epoch, i);
        delay_before_probe(next_delay(&state));
        publish_slots(init_slot, alloc_slot);
        atomic_store_explicit(&done_writer.value, i, memory_order_release);
    }
    return NULL;
}

static void *reader_worker(void *unused) {
    (void)unused;
    uint32_t state = UINT32_C(0x713b712a);
    for (uint64_t i = 1; i <= iterations; ++i) {
        wait_epoch(&start_epoch, i);
        delay_before_probe(next_delay(&state));
        reader(init_slot, alloc_slot, &result);
        atomic_store_explicit(&done_reader.value, i, memory_order_release);
    }
    return NULL;
}

static double monotonic_seconds(void) {
    struct timespec t;
    if (clock_gettime(CLOCK_MONOTONIC, &t)) exit(2);
    return (double)t.tv_sec + (double)t.tv_nsec / 1e9;
}

// These are serial observations of explicitly seeded states, not concurrency results.
static int selftest(void) {
    reader_fn functions[] = {read_original, read_barrier};
    const char *names[] = {"original", "barrier"};
    const uint64_t states[][2] = {{0, 0}, {1, 1}, {0, 1}};
    for (size_t r = 0; r < 2; ++r) {
        for (size_t s = 0; s < 3; ++s) {
            seed_slots(init_slot, alloc_slot, states[s][0], states[s][1]);
            functions[r](init_slot, alloc_slot, &result);
            bool ok = result.guard == states[s][1] && result.callback == states[s][0];
            printf("{\"kind\":\"serial_selftest\",\"reader\":\"%s\","
                   "\"seed_init\":%" PRIu64 ",\"seed_alloc\":%" PRIu64 ","
                   "\"guard\":%" PRIu64 ",\"callback\":%" PRIu64 ",\"ok\":%s}\n",
                   names[r], states[s][0], states[s][1], result.guard,
                   result.callback, ok ? "true" : "false");
            if (!ok) return 2;
        }
    }
    return 0;
}

int main(int argc, char **argv) {
    setvbuf(stdout, NULL, _IOLBF, 0);
    signal(SIGALRM, timeout_handler);
    alarm(30);
    if (argc == 2 && strcmp(argv[1], "selftest") == 0) return selftest();
    if (argc != 3 || (strcmp(argv[1], "original") && strcmp(argv[1], "barrier"))) {
        fprintf(stderr, "usage: native-litmus {selftest|{original|barrier} iterations}\n");
        return 2;
    }
    char *end = NULL;
    iterations = strtoull(argv[2], &end, 10);
    if (*end || iterations == 0 || iterations > 1000000) return 2;
    reader = strcmp(argv[1], "original") == 0 ? read_original : read_barrier;
    int translated = -1;
    size_t len = sizeof(translated);
    if (sysctlbyname("sysctl.proc_translated", &translated, &len, NULL, 0)) translated = -1;
    if (translated != 0 || !atomic_is_lock_free(&start_epoch.value)) return 2;
    printf("{\"kind\":\"configuration\",\"reader\":\"%s\",\"iterations\":%" PRIu64
           ",\"proc_translated\":%d,\"slot_distance\":%zu,\"init_mod128\":%zu,"
           "\"alloc_mod128\":%zu,\"delay_max\":63,\"watchdog_seconds\":30}\n",
           argv[1], iterations, translated, (size_t)((char *)alloc_slot - (char *)init_slot),
           (size_t)((uintptr_t)init_slot % 128), (size_t)((uintptr_t)alloc_slot % 128));
    pthread_t writer_thread, reader_thread;
    if (pthread_create(&writer_thread, NULL, writer_worker, NULL) ||
        pthread_create(&reader_thread, NULL, reader_worker, NULL)) return 2;
    uint64_t guard_zero = 0, published = 0, inconsistent = 0;
    double begin = monotonic_seconds();
    for (uint64_t i = 1; i <= iterations; ++i) {
        // Previous iteration's two done acquisitions precede every reset.
        reset_slots(init_slot, alloc_slot);
        atomic_store_explicit(&start_epoch.value, i, memory_order_release);
        wait_epoch(&done_writer, i);
        wait_epoch(&done_reader, i);
        if (result.guard == 0 && result.callback == 0) ++guard_zero;
        else if (result.guard == 1 && result.callback == 1) ++published;
        else if (result.guard == 1 && result.callback == 0) {
            ++inconsistent;
            if (inconsistent <= 8) {
                printf("{\"kind\":\"witness\",\"iteration\":%" PRIu64
                       ",\"guard\":1,\"callback\":0}\n", i);
            }
            if (reader == read_barrier) {
                fprintf(stderr, "barrier control observed an inconsistent pair; stop and audit\n");
                return 3;
            }
        } else {
            fprintf(stderr, "unexpected guard/callback value; stop and audit\n");
            return 2;
        }
        if (i % 250000 == 0 || i == iterations) {
            printf("{\"kind\":\"%s\",\"completed\":%" PRIu64 ",\"guard_zero\":%" PRIu64
                   ",\"published\":%" PRIu64 ",\"inconsistent\":%" PRIu64 ",\"seconds\":%.6f}\n",
                   i == iterations ? "result" : "progress", i, guard_zero, published,
                   inconsistent, monotonic_seconds() - begin);
        }
    }
    if (pthread_join(writer_thread, NULL) || pthread_join(reader_thread, NULL)) return 2;
    alarm(0);
    return 0;
}
