/* R exposes interrupt conditions but no R-level SIGTERM/SIGHUP receiver. This
 * adapter records cancellation only; processx/ps still own all child control. */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <signal.h>

static volatile sig_atomic_t pending = 0;
static struct sigaction previous[3];
static const int watched[3] = {SIGINT, SIGTERM, SIGHUP};
static int installed = 0;

static void receive_signal(int number) { pending = number; }

static SEXP install_signals(void) {
    if (installed) Rf_error("Cancellation receiver is already installed.");
    struct sigaction action;
    action.sa_handler = receive_signal;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    for (int i = 0; i < 3; ++i) {
        if (sigaction(watched[i], &action, &previous[i]) != 0) {
            while (i-- > 0) sigaction(watched[i], &previous[i], NULL);
            Rf_error("Cannot establish cancellation receiver.");
        }
    }
    pending = 0;
    installed = 1;
    return R_NilValue;
}

static SEXP poll_signals(void) { return Rf_ScalarInteger((int) pending); }

static SEXP restore_signals(void) {
    if (installed) {
        for (int i = 0; i < 3; ++i)
            if (sigaction(watched[i], &previous[i], NULL) != 0)
                Rf_error("Cannot restore cancellation receiver.");
        installed = 0;
    }
    return R_NilValue;
}

static const R_CallMethodDef calls[] = {
    {"install_signals", (DL_FUNC) &install_signals, 0},
    {"poll_signals", (DL_FUNC) &poll_signals, 0},
    {"restore_signals", (DL_FUNC) &restore_signals, 0},
    {NULL, NULL, 0}
};

void R_init_review_ready_signals(DllInfo *dll) {
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
