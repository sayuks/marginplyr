// Execute the shipped workflow script against the external GitHub API boundary.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
const handler = new AsyncFunction('github', 'context', 'core', 'setTimeout',
  fs.readFileSync(process.argv[2], 'utf8'));
// These fixtures evaluate the committed native expressions, not a second key builder.
const lifetimes = JSON.parse(fs.readFileSync(process.argv[3], 'utf8'));
const evaluate = (text, github) => text.replace(/\$\{\{(.*?)\}\}/g, (_, expression) =>
  new Function('github', 'format', `return (${expression});`)(github,
    (pattern, value) => pattern.replace('{0}', value)));
for (const [path, workflow] of Object.entries(lifetimes)) {
  const group = (ref, event_name = 'pull_request', run_id = 1) => evaluate(workflow.concurrency.group,
    {workflow: workflow.name, ref, event_name, run_id});
  const pr = group('refs/pull/42/merge');
  assert.equal(pr, group('refs/pull/42/merge', 'pull_request', 2));
  assert.notEqual(pr, group('refs/pull/43/merge'));
  assert.notEqual(pr, group('refs/heads/main', 'push'));
  assert.equal(group('refs/heads/main', 'push', 1), group('refs/heads/main', 'push', 2));
  assert.notEqual(group('refs/heads/main', 'push'), group('refs/heads/main', 'workflow_dispatch'));
  assert.notEqual(group('refs/heads/main', 'workflow_dispatch', 1),
    group('refs/heads/main', 'workflow_dispatch', 2));
  const prefix = path === 'altdoc.yaml' ? 'altdoc-build' : workflow.name;
  const dispatched = ['altdoc.yaml', 'postgres-transactions.yaml', 'release-matrix.yaml'].includes(path);
  const automatic = dispatched ? `${prefix}-auto` : prefix;
  assert.equal(pr, `${automatic}-refs/pull/42/merge`);
  assert.equal(group('refs/heads/main', 'push'), `${automatic}-refs/heads/main`);
  if (dispatched) {
    // A pre-change manual run used the bare workflow/ref group.
    assert.notEqual(group('refs/heads/main', 'push'), `${prefix}-refs/heads/main`);
  }
  assert.equal(evaluate(workflow.concurrency['cancel-in-progress'], {event_name: 'pull_request'}), 'true');
  assert.equal(evaluate(workflow.concurrency['cancel-in-progress'], {event_name: 'workflow_dispatch'}), 'false');
}
console.log('Same-PR and main updates share groups; different PRs and legacy/manual runs remain independent.');
const closed = {
  number: 42, state: 'closed', closed_at: '2026-11-09T05:00:00Z',
  head: { ref: 'topic', repo: { id: 20, owner: {login: 'forker'} } }, base: { repo: { id: 10 } }
};
const run = (id, overrides = {}) => ({
  id, event: 'pull_request', status: 'queued', run_attempt: 1,
  path: '.github/workflows/R-CMD-check.yaml', head_branch: 'topic',
  head_sha: 'old-revision', head_repository: { id: 20 },
  repository: { id: 10 }, created_at: '2026-11-09T04:00:00Z',
  pull_requests: [{ number: 42, base: { repo: { id: 10 } } }], ...overrides
});
async function scenario(options = {}) {
  const runs = options.runs || [run(1)];
  const cancelled = [], failures = [], messages = [];
  let prReads = 0, listReads = 0, runReads = 0;
  const github = { rest: {
    pulls: {
      get: async () => ({ data: options.liveAt ? options.liveAt(++prReads) : options.live || closed }),
      list: async ({state, head}) => {
        assert.equal(state, 'all');
        assert.equal(head, 'forker:topic');
        return {data: options.associated || [options.eventPr || closed]};
      },
      listCommits: async ({page}) => ({data: options.historyPages ?
        Array.from({length: 100}, (_, i) => ({sha: `${page}-${i}`})) :
        options.commits || [{sha: 'old-revision'}]})
    },
    actions: {
      listWorkflowRunsForRepo: async ({status, page, branch, event}) => {
        listReads++;
        assert.equal(branch, 'topic');
        assert.equal(event, 'pull_request');
        return {data: {
          total_count: options.tooManyPages ? 1000 : runs.filter(r => r.status === status).length,
          workflow_runs: page === 1 ? runs.filter(r => r.status === status) : []
        }};
      },
      getWorkflowRun: async ({run_id}) => {
        runReads++;
        if (options.getError) throw new Error('API read unavailable');
        const original = runs.find(r => r.id === run_id);
        if (options.refresh) return {data: options.refresh(original, cancelled.includes(run_id))};
        return {data: cancelled.includes(run_id) && !options.stuck ?
          {...original, status: 'completed'} : original};
      },
      cancelWorkflowRun: async ({run_id}) => {
        if (options.cancelError) throw Object.assign(new Error('API cancellation rejected'), {status: options.cancelError});
        cancelled.push(run_id);
      }
    }
  }};
  const context = {repo: {owner: 'owner', repo: 'repo'}, eventName: 'pull_request_target',
    runId: 99, payload: {action: 'closed', number: 42, pull_request: options.eventPr || closed,
      repository: {id: 10}}};
  const core = {info: x => messages.push(x), warning: x => messages.push(x),
    setFailed: x => failures.push(x)};
  await handler(github, context, core, resolve => resolve());
  return {cancelled, failures, messages, listReads, runReads};
}
(async () => {
  const result = await scenario();
  assert.deepEqual(result.cancelled, [1]);
  assert.deepEqual(result.failures, []);
  console.log('Closed PR retires its queued automatic run.');
  const isolated = await scenario({runs: [run(1), run(2, {status: 'in_progress'}),
    run(3, {pull_requests: [{number: 43, base: {repo: {id: 10}}}]}),
    run(4, {event: 'push'}), run(5, {event: 'workflow_dispatch'}),
    run(6, {path: '.github/workflows/rhub.yaml'}),
    run(7, {status: 'completed'}), run(99),
    run(8, {path: '.github/workflows/retire-pr-ci.yaml'}),
    run(9, {head_repository: {id: 30}}),
    run(10, {created_at: '2026-11-09T06:00:00Z'}),
    run(11, {repository: {id: 30}}), run(15, {status: 'waiting'}),
    run(16, {status: 'pending'}), run(17, {status: 'requested'})]});
  assert.deepEqual(isolated.cancelled, [1, 2, 15, 16, 17]);
  assert.deepEqual(isolated.failures, []);
  console.log('Other PRs, main, manual release, completed runs, handler and later runs are isolated.');
  for (const state of [closed, {...closed, head: {...closed.head, repo: {id: 30, owner: {login: 'forker'}}}}]) {
    const fork = await scenario({runs: [run(12, {pull_requests: [], head_repository: state.head.repo})],
      associated: [state], eventPr: state});
    assert.deepEqual(fork.cancelled, [12]);
    assert.deepEqual(fork.failures, []);
  }
  for (const associated of [[], [closed, {...closed, number: 43}],
    [{...closed, head: {...closed.head, ref: 'other'}}]]) {
    const ambiguous = await scenario({runs: [run(13, {pull_requests: []})], associated});
    assert.deepEqual(ambiguous.cancelled, []);
    assert.equal(ambiguous.failures.length, 1);
    assert.match(ambiguous.failures[0], /Cannot identify PR/);
  }
  console.log('Fork and legacy runs require positive, unambiguous API association.');
  for (const options of [{commits: []}, {historyPages: true},
    {associated: Array.from({length: 100}, () => closed)}]) {
    const missing = await scenario({...options, runs: [run(14, {pull_requests: []})]});
    assert.deepEqual(missing.cancelled, []);
    assert.match(missing.failures[0], /Cannot identify PR/);
  }
  const multiple = await scenario({runs: [run(18, {pull_requests: [
    {number: 42}, {number: 43}]})]});
  assert.deepEqual(multiple.cancelled, []);
  assert.match(multiple.failures[0], /multiple run associations/);
  const deletedFork = await scenario({eventPr: {...closed, head: {...closed.head, repo: null}}});
  assert.deepEqual(deletedFork.cancelled, []);
  assert.match(deletedFork.failures[0], /head repository is unavailable/);
  for (const live of [{...closed, state: 'open'}, {...closed, closed_at: '2026-11-09T07:00:00Z'}]) {
    assert.deepEqual((await scenario({live})).cancelled, []);
    assert.deepEqual((await scenario({liveAt: count => count === 1 ? closed : live})).cancelled, []);
  }
  const partialReopen = await scenario({runs: [run(1), run(2)],
    liveAt: count => count < 3 ? closed : {...closed, state: 'open'}});
  assert.deepEqual(partialReopen.cancelled, [1]);
  assert.deepEqual(partialReopen.failures, []);
  console.log('Reopening or a later close invalidates stale close events, including during retirement.');
  const finished = await scenario({refresh: original => ({...original, status: 'completed'})});
  assert.deepEqual(finished.cancelled, []);
  const conflict = await scenario({cancelError: 409});
  // A 409 is acceptable only when a subsequent read establishes completion.
  let reads = 0;
  const naturallyFinished = await scenario({cancelError: 409, refresh: original =>
    ++reads === 1 ? original : {...original, status: 'completed', conclusion: 'success'}});
  assert.deepEqual(naturallyFinished.failures, []);
  assert.equal(conflict.failures.length, 1);
  const stuck = await scenario({stuck: true});
  assert.equal(stuck.failures.length, 1);
  assert.match(stuck.failures[0], /polling limit/);
  assert.equal(stuck.runReads, 13);
  const rerun = await scenario({refresh: (original, cancelled) =>
    ({...original, run_attempt: cancelled ? 2 : 1})});
  assert.match(rerun.failures[0], /changed attempts/);
  const refreshRerun = await scenario({refresh: original => ({...original, run_attempt: 2})});
  assert.deepEqual(refreshRerun.cancelled, []);
  console.log('Natural completion, cancellation completion and attempt changes are observed with bounded polling.');
  for (const options of [{cancelError: 403}, {getError: true}]) {
    const failed = await scenario(options);
    assert.equal(failed.failures.length, 1);
    assert.match(failed.failures[0], /Unable to retire/);
  }
  const paged = await scenario({tooManyPages: true});
  assert.equal(paged.listReads, 15);
  assert.match(paged.failures[0], /pagination limit/);
  const budget = await scenario({runs: Array.from({length: 51}, (_, i) => run(i + 100))});
  assert.deepEqual(budget.cancelled, []);
  assert.match(budget.failures[0], /budget exceeded/);
  console.log('API failures and pagination/target limits fail visibly without broad cancellation.');
})().catch(error => { console.error(error); process.exitCode = 1; });
