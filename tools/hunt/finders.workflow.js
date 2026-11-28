// Finder lanes of a bug hunt (design/agents/bug-hunt.md, step 4).
//
// Workflow({ scriptPath, args: { scratch, lanes } }), where `scratch` is the
// session's scratch directory holding brief-finder.md and lanes/<id>/brief.md,
// and each lane is { id, name, model, effort }. A lane that returns nothing is
// reported as failed and is not run on another model.
export const meta = {
  name: 'marginplyr-hunt-finders',
  description: 'Independent read-only finders hunt undiscovered marginplyr defects, one starting point each',
  phases: [{ title: 'Find', detail: 'one agent per lane, no shared candidates' }],
}
const S = args.scratch
const text = { type: 'string' }
const CANDIDATE = {
  type: 'object',
  properties: {
    title: text, contract_source: text, repro_script: text, repro_code: text,
    expected: text, observed: text, native_control: text,
    executed_backends: text, rendered_only_backends: text, cause_file: text,
    cause_function: text, cause: text,
    direction: { type: 'string', enum: ['wrong_value', 'wrong_refusal', 'missing_refusal', 'wrong_condition', 'untimely_query', 'doc_mismatch', 'other'] },
    confidence: text, severity: text, counterevidence: text, not_tried: text,
  },
  required: ['title', 'contract_source', 'repro_script', 'repro_code', 'expected', 'observed', 'native_control', 'executed_backends', 'rendered_only_backends', 'cause_file', 'cause_function', 'cause', 'direction', 'confidence', 'severity'],
}
const RETURN = {
  type: 'object',
  properties: {
    lane: text,
    candidates: { type: 'array', items: CANDIDATE },
    not_reproduced: { type: 'array', items: text },
    covered: { type: 'array', items: text },
    not_covered: { type: 'array', items: text },
    lane_notes: text,
  },
  required: ['lane', 'candidates', 'not_reproduced', 'covered', 'not_covered', 'lane_notes'],
}
phase('Find')
const results = await parallel(args.lanes.map(l => () =>
  agent(
    'You are investigator lane ' + l.id + ' (' + l.name + ') of a defect hunt on the R package marginplyr. ' +
    'Read these two files completely before doing anything else, and follow them exactly:\n' +
    '1. ' + S + '/brief-finder.md (rules, method, the native-control lesson, report format)\n' +
    '2. ' + S + '/lanes/' + l.id + '/brief.md (your starting point)\n\n' +
    'Your lane directory is ' + S + '/lanes/' + l.id + '/ . The checkout in the current directory is read-only for you. ' +
    'Read the named source and contract texts first, then probe with small Rscript files against the installed build. ' +
    'There is no quota of candidates; an empty list is acceptable, and a candidate needs a fresh-process reproduction with a native control on the same backend. ' +
    'Write no report file: return the structured result with lane = "' + l.id + '".',
    { label: 'lane ' + l.id + ': ' + l.name, phase: 'Find', model: l.model, effort: l.effort, schema: RETURN }
  ).then(r => r ? r : { lane: l.id, failed: true })
))
return results
