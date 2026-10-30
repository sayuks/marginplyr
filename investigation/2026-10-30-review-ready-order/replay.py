"""Replay four-worker queues with each recorded duration sample held fixed."""
import csv
import heapq
from pathlib import Path

root = Path(__file__).resolve().parent
samples = {}
for name in ('baseline', 'descending'):
    with (root / name / 'files.csv').open() as stream:
        samples[name] = list(csv.DictReader(stream))

old_order = [
    'test-sqlite-wide-plan-threshold.R',
    'test-sql-expansion-input-limits.R',
    'test-sqlite-wide-expansion.R',
    'test-query-policy.R',
]
all_files = {row['file'] for row in samples['baseline']}
old_order += sorted(all_files - set(old_order))
new_order = [row['file'] for row in samples['baseline']]

for sample, rows in samples.items():
    durations = {row['file']: float(row['duration_s']) for row in rows}
    for name, order in [('old', old_order), ('descending', new_order)]:
        workers = [(0, worker) for worker in range(4)]
        completions = []
        for file in order:
            available, worker = heapq.heappop(workers)
            end = available + durations[file]
            heapq.heappush(workers, (end, worker))
            completions.append(end)
        completions.sort()
        print(sample, name,
              f'elapsed={completions[-1]:.3f}',
              f'tail={completions[-1] - completions[-2]:.3f}')
