from pathlib import Path
import hashlib
import json
import platform
import subprocess
import sys

base = Path(__file__).resolve().parent
handlers = ['mixed', 'calling_and_exiting', 'condition_only', 'error_only', 'calling_return', 'global_return', 'no_handler']
callbacks = ['default', 'error_function', 'error_expression', 'interrupt_function', 'both']
routes = ['rich_then_native', 'native']
results = []
for handler in handlers:
    for callback in callbacks:
        for route in routes:
            command = ['Rscript', '--vanilla', str(base / 'probe.R'), route, handler, callback]
            result = subprocess.run(command, text=True, capture_output=True, timeout=15)
            events = [json.loads(line) for line in result.stdout.splitlines() if line.startswith('{')]
            entry = dict(route=route, handler=handler, callbacks=callback, command=command, exit_code=result.returncode, stdout=result.stdout, stderr=result.stderr, events=events)
            results.append(entry)
(base / 'results.json').write_text(json.dumps(results, indent=2) + '\n')
failures = []
for entry in results:
    names = [e['event'] for e in entry['events']]
    if 'error_handler' in names or 'returned_from_signaler' in names or 'after_error_only' in names:
        failures.append(entry)
    if entry['handler'] in ['mixed', 'calling_and_exiting', 'condition_only']:
        enriched = entry['route'] == 'rich_then_native'
        handled = next(e for e in entry['events'] if e['event'] == 'handled')
        if handled['rich'] != enriched or entry['exit_code'] != 0:
            failures.append(entry)
        for event in entry['events']:
            if event['event'] in ['interrupt_handler', 'condition_handler'] and enriched:
                if not all(event[k] for k in ['rich', 'parent_retained', 'original_interrupt_retained', 'replay_retained', 'original_error_untouched']) or event['inherits_error']:
                    failures.append(entry)
# Default actions and callbacks after no exiting handler must match the native control.
for handler in ['error_only', 'calling_return', 'global_return', 'no_handler']:
    for callback in callbacks:
        pair = [e for e in results if e['handler'] == handler and e['callbacks'] == callback]
        rich, native = pair
        rich_callbacks = [e['event'] for e in rich['events'] if 'option' in e['event']]
        native_callbacks = [e['event'] for e in native['events'] if 'option' in e['event']]
        if rich['exit_code'] != native['exit_code'] or rich_callbacks != native_callbacks:
            failures.extend(pair)
        if handler in ['calling_return', 'global_return']:
            events = [e for e in rich['events'] if e['event'] == 'calling_handler']
            if len(events) != 2 or not events[0]['rich'] or events[1]['rich']:
                failures.append(rich)
summary = dict(cases=len(results), failure_count=len(failures), accepted=not failures, observations=dict(exiting_handlers='rich object and original cause chain retained', error_only='never called; fallback matches native process/callback outcome', calling_return='one rich notification then one bare native notification', default_callbacks='same as native for default, function/expression error options, interrupt option, and both'), limitations=['Controlled rlang interrupts only; no new SIGINT or whole resource-owner implementation tested', 'Returning calling handlers observe native fallback as a second bare interrupt', 'Handler-initiated nonlocal transfers or errors were not tested', 'R 4.6.1 Darwin arm64 and installed dependencies only'])
(base / 'summary.json').write_text(json.dumps(summary, indent=2) + '\n')
manifest = dict(python=sys.version, platform=platform.platform(), scripts={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in [base/'probe.R',base/'run.py']})
(base / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
print(json.dumps(summary, indent=2))
if failures:
    print(json.dumps(failures, indent=2))
    raise SystemExit(1)
