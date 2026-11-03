#!/usr/bin/env python3
"""Analyze user-requested repeated full checks; no runtime ordering state."""
import argparse
import csv
import heapq
import itertools
import json
import re
import statistics
from collections import Counter, defaultdict
from pathlib import Path


def write_csv(path, rows):
    if not rows:
        return
    with path.open('w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def load_run(run, expected, root=None):
    directory = Path(run['artifact_directory'])
    if root is not None and (root / f"run-{run['number']}").is_dir():
        directory = root / f"run-{run['number']}"
    paths = sorted((directory / 'check' / 'file-timings').glob('*.tsv'))
    events = defaultdict(list)
    pids = set()
    for path in paths:
        with path.open() as handle:
            for row in csv.reader(handle, delimiter='\t'):
                if len(row) != 5:
                    raise ValueError(f'Malformed event in {path}: {row!r}')
                event, name, pid, utc, elapsed = row
                if event not in ('start', 'end'):
                    raise ValueError(f'Unknown event: {event}')
                events[name].append((event, int(pid), float(utc), float(elapsed)))
                pids.add(int(pid))
    if set(events) != set(expected):
        raise ValueError(f'Wrong file set for run {run["number"]}: missing {set(expected)-set(events)}, extra {set(events)-set(expected)}')
    if len(pids) != 4 or len(paths) != 4:
        raise ValueError(f'Expected four worker logs, got {len(paths)} / {len(pids)}')
    durations, rows = {}, []
    for name in expected:
        pair = events[name]
        if len(pair) != 2 or [row[0] for row in pair] != ['start', 'end'] or pair[0][1] != pair[1][1]:
            raise ValueError(f'Wrong start/end pairing: {name}: {pair}')
        start, end = pair
        duration = end[3] - start[3]
        if not 0 <= duration < float('inf'):
            raise ValueError(f'Invalid duration for {name}: {duration}')
        durations[name] = round(duration, 6)
        rows.append({'run': run['number'], 'file': name, 'worker_pid': start[1],
                     'start_utc': start[2], 'end_utc': end[2],
                     'start_worker_s': start[3], 'end_worker_s': end[3],
                     'duration_s': round(duration, 6)})
    first = min(row['start_utc'] for row in rows)
    last = max(row['end_utc'] for row in rows)
    ends = sorted(row['end_utc'] for row in rows)
    console = (directory / 'console.log').read_text()
    count_matches = re.findall(r'R CMD check: (\d+) ERROR\(s\), (\d+) WARNING\(s\), (\d+) NOTE\(s\)', console)
    if not count_matches:
        raise ValueError(f'No terminal native check counts for run {run["number"]}')
    return durations, rows, {'run': run['number'], 'session_id': run['session_id'],
        'exit_code': run['exit_code'], 'files': len(rows), 'workers': len(pids),
        'worker_span_s': round(last-first, 6), 'last_file_tail_s': round(ends[-1]-ends[-2], 6),
        'sum_file_s': round(sum(durations.values()), 6),
        'errors': int(count_matches[-1][0]), 'warnings': int(count_matches[-1][1]),
        'notes': int(count_matches[-1][2]), 'artifact_directory': run['artifact_directory']}


def stable_promotions(order, durations, relative, absolute):
    result, promoted, crossings = [], [], []
    for name in order:
        result.append(name)
        index = len(result)-1
        moved = False
        while index:
            before = result[index-1]
            if not (durations[name] > (1+relative)*durations[before]
                    and durations[name]-durations[before] > absolute):
                break
            result[index-1], result[index] = name, before
            crossings.append((name, before))
            moved = True
            index -= 1
        if moved:
            promoted.append(name)
    return result, promoted, crossings


def replay(order, durations):
    workers = [(0.0, index) for index in range(4)]
    heapq.heapify(workers)
    for name in order:
        available, worker = heapq.heappop(workers)
        heapq.heappush(workers, (available+durations[name], worker))
    ends = sorted(time for time, _ in workers)
    return ends[-1], ends[-1]-ends[-2]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parent)
    parser.add_argument('--partial', action='store_true')
    args = parser.parse_args()
    root = args.root
    state = json.loads((root / 'executions.json').read_text())
    files = list(csv.DictReader((root / 'files.csv').open()))
    order = [row['file'] for row in files]
    new = {row['file'] for row in files if row['registered'] == 'FALSE'}
    runs = [run for run in state['runs'] if run.get('status') == 'passed' and run.get('exit_code') == 0]
    if len(runs) != 3 and not args.partial:
        raise SystemExit(f'Require three collected successful terminal results; have {len(runs)}')
    if not runs:
        print('No collected successful measurement runs yet.')
        return
    data, all_rows, run_summaries = [], [], []
    for run in runs:
        durations, rows, summary = load_run(run, order, root=root)
        data.append(durations)
        all_rows.extend(rows)
        run_summaries.append(summary)
    write_csv(root / 'file-intervals.csv', all_rows)
    write_csv(root / 'run-summary.csv', run_summaries)
    if len(data) < 3:
        print(json.dumps({'validated_runs': run_summaries}, indent=2))
        return
    variability = []
    for name in order:
        values = [durations[name] for durations in data]
        median = statistics.median(values)
        variability.append({'file': name, 'registered': name not in new,
            'run_1_s': round(values[0], 6), 'run_2_s': round(values[1], 6),
            'run_3_s': round(values[2], 6), 'median_s': round(median, 6),
            'max_min_ratio': round(max(values)/min(values), 6) if min(values) else None,
            'range_fraction_of_median': round((max(values)-min(values))/median, 6) if median else None})
    write_csv(root / 'file-variability.csv', sorted(variability, key=lambda row: -row['median_s']))
    margins = [(0.5, 10), (0.5, 5), (0.25, 10), (0.25, 5), (0.2, 5), (0.1, 2), (0.0, 0)]
    proposal_rows, replay_rows, policy_rows, counterfactual_rows, sequential_rows = [], [], [], [], []
    medians = {name: statistics.median([d[name] for d in data]) for name in order}
    heaviest = max(order, key=medians.get)
    late_heavy = [name for name in order if name != heaviest] + [heaviest]
    for relative, absolute in margins:
        policy = f'{relative:g}-relative+{absolute:g}s'
        proposals, crossings_by_run, movements, promotions = [], [], [], []
        for source_index, durations in enumerate(data, 1):
            candidate, promoted, crossings = stable_promotions(order, durations, relative, absolute)
            proposals.append(candidate)
            crossings_by_run.append(set(crossings))
            movements.append(sum(a != b for a, b in zip(order, candidate)))
            promotions.append(set(promoted))
            for position, name in enumerate(candidate, 1):
                proposal_rows.append({'policy': policy, 'measured_run': source_index,
                    'position': position, 'file': name, 'duration_s': round(durations[name], 6),
                    'unregistered': name in new, 'promoted': name in promoted})
            for evaluation_index, evaluation in enumerate(data, 1):
                before, before_tail = replay(order, evaluation)
                after, after_tail = replay(candidate, evaluation)
                replay_rows.append({'policy': policy, 'measured_run': source_index,
                    'evaluated_run': evaluation_index, 'held_out': source_index != evaluation_index,
                    'baseline_s': round(before, 6), 'candidate_s': round(after, 6),
                    'gain_s': round(before-after, 6), 'gain_percent': round(100*(before-after)/before, 6),
                    'baseline_tail_s': round(before_tail, 6), 'candidate_tail_s': round(after_tail, 6)})
            late_candidate, _, _ = stable_promotions(late_heavy, durations, relative, absolute)
            for evaluation_index, evaluation in enumerate(data, 1):
                before, _ = replay(late_heavy, evaluation)
                after, _ = replay(late_candidate, evaluation)
                counterfactual_rows.append({'policy': policy, 'measured_run': source_index,
                    'evaluated_run': evaluation_index, 'held_out': source_index != evaluation_index,
                    'late_file': heaviest, 'promoted_position': late_candidate.index(heaviest)+1,
                    'baseline_s': round(before, 6), 'candidate_s': round(after, 6),
                    'gain_s': round(before-after, 6)})
        agreement = []
        for left, right in itertools.combinations(crossings_by_run, 2):
            agreement.append(len(left & right)/len(left | right) if left | right else 1.0)
        union = set.union(*crossings_by_run)
        stable_all = sum(all(data[i][fast] > (1+relative)*data[i][slow]
                             and data[i][fast]-data[i][slow] > absolute
                             for i in range(3)) for fast, slow in union)
        held_out = [row for row in replay_rows if row['policy'] == policy and row['held_out']]
        sequential_updates = []
        for sequence in itertools.permutations(range(3)):
            current = order[:]
            later_updates = 0
            for step, sample_index in enumerate(sequence, 1):
                candidate, promoted, crossed = stable_promotions(current, data[sample_index], relative, absolute)
                changed = sum(a != b for a, b in zip(current, candidate))
                if step > 1 and candidate != current:
                    later_updates += 1
                sequential_rows.append({'policy': policy, 'sample_sequence': '-'.join(str(i+1) for i in sequence),
                    'step': step, 'measured_run': sample_index+1, 'moved_positions': changed,
                    'promoted_files': len(promoted), 'crossings': len(crossed)})
                current = candidate
            sequential_updates.append(later_updates)
        policy_rows.append({'policy': policy, 'relative_margin': relative, 'absolute_margin_s': absolute,
            'sequential_post_first_updates_min': min(sequential_updates),
            'sequential_post_first_updates_max': max(sequential_updates),
            'moved_positions_run_1': movements[0], 'moved_positions_run_2': movements[1],
            'moved_positions_run_3': movements[2],
            'promoted_files_run_1': len(promotions[0]), 'promoted_files_run_2': len(promotions[1]),
            'promoted_files_run_3': len(promotions[2]),
            'new_promoted_each_run': '/'.join(str(len(p & new)) for p in promotions),
            'mean_crossing_agreement': round(statistics.mean(agreement), 6),
            'crossing_union': len(union), 'crossings_supported_all_3': stable_all,
            'held_out_median_gain_s': round(statistics.median(row['gain_s'] for row in held_out), 6),
            'held_out_min_gain_s': min(row['gain_s'] for row in held_out),
            'held_out_max_gain_s': max(row['gain_s'] for row in held_out)})
    current = order[:]
    chronological = []
    for index, durations in enumerate(data, 1):
        candidate, promoted, crossings = stable_promotions(current, durations, .5, 10)
        assert stable_promotions(candidate, durations, .5, 10)[0] == candidate
        before, _ = replay(current, durations)
        after, _ = replay(candidate, durations)
        chronological.append({'run': index, 'promoted': promoted, 'crossings': crossings,
                              'model_gain_s': before-after})
        current = candidate
    (root / 'chronological-updates.json').write_text(json.dumps(chronological, indent=2)+'\n')
    write_csv(root / 'proposals.csv', proposal_rows)
    write_csv(root / 'replay.csv', replay_rows)
    write_csv(root / 'policy-comparison.csv', policy_rows)
    write_csv(root / 'late-heavy-counterfactual.csv', counterfactual_rows)
    write_csv(root / 'sequential-updates.csv', sequential_rows)
    slow = [row for row in variability if row['median_s'] >= 1]
    summary = {'sha': state['sha'], 'runs': run_summaries, 'unregistered_files': sorted(new),
        'heaviest': heaviest, 'policies': policy_rows,
        'slow_file_variation': {'files': len(slow),
            'median_max_min_ratio': statistics.median(row['max_min_ratio'] for row in slow),
            'maximum_max_min_ratio': max(row['max_min_ratio'] for row in slow)},
        'top_files': sorted(variability, key=lambda row: -row['median_s'])[:12],
        'limitations': ['Three local observations, not a confidence interval or CI-host calibration.',
            'Counterfactual schedules are four-worker greedy models with held durations, not extra observed suite runs.',
            'No cold-cache reset, CPU pinning or exclusion of allowed focused work was imposed.']}
    (root / 'analysis-summary.json').write_text(json.dumps(summary, indent=2)+'\n')
    print(json.dumps(summary, indent=2))


if __name__ == '__main__':
    main()
