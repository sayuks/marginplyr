import csv
import json
from pathlib import Path
import sys

directory = Path(sys.argv[1])
source = Path(sys.argv[2])
expected = {p.name for p in (source / 'tests/testthat').glob('test-*.R')}
events = []
for path in directory.glob('*.tsv'):
    with path.open() as stream:
        for kind, file, pid, utc, elapsed in csv.reader(stream, delimiter='\t'):
            events.append(dict(kind=kind, file=file, pid=int(pid),
                               utc=float(utc), elapsed=float(elapsed)))
files = {}
for event in events:
    key = (event['file'], event['kind'])
    assert key not in files, ('duplicate', key)
    files[key] = event
assert {event['file'] for event in events} == expected
assert len(events) == len(expected) * 2
assert len({event['pid'] for event in events}) == 4
origin = min(event['utc'] for event in events)
rows = []
for file in sorted(expected):
    start, end = files[(file, 'start')], files[(file, 'end')]
    assert start['pid'] == end['pid']
    assert end['elapsed'] >= start['elapsed']
    rows.append(dict(file=file, worker=start['pid'],
                     start_utc=start['utc'], end_utc=end['utc'],
                     start_s=start['utc']-origin, end_s=end['utc']-origin,
                     duration_s=end['elapsed']-start['elapsed']))
rows.sort(key=lambda row: (-row['duration_s'], row['file']))
with (directory / 'files.csv').open('w') as stream:
    writer = csv.DictWriter(stream, fieldnames=list(rows[0]), lineterminator='\n')
    writer.writeheader()
    writer.writerows(rows)
ends = sorted(row['end_s'] for row in rows)
summary = dict(files=len(rows), workers=4, origin_utc=origin,
               elapsed_s=ends[-1], tail_after_penultimate_s=ends[-1]-ends[-2],
               last_four=sorted(rows, key=lambda row: row['end_s'])[-4:],
               longest=rows[:20])
print(json.dumps(summary, indent=2))
