// Refutation of a bug hunt's candidates (design/agents/bug-hunt.md, step 6).
//
// Workflow({ scriptPath, args: { scratch, groups } }), where `scratch` holds
// brief-refuter.md and cands/<id>.md, and each group is { id, refuter }: one
// cause group, named by its first candidate, and the model that refutes it.
// A refuter that returns nothing is tried once more on the same model and
// then reported as failed. It is never moved to another model here: a usage
// limit fails the same way as any other error, and the coordinator reads
// which it was before choosing.
export const meta = {
  name: 'marginplyr-hunt-verify',
  description: 'One fresh-context refuter per cause group, on a model other than the finder\'s',
  phases: [{ title: 'Refute', detail: 'argue that each candidate is not a defect' }],
}
const S = args.scratch
const text = { type: 'string' }
const RETURN = {
  type: 'object',
  properties: {
    id: text,
    verdict: { type: 'string', enum: ['refuted', 'narrowed', 'stands'] },
    reproduced: text, native_control: text, intended_evidence: text,
    contract_evidence: text, cause_reading: text, boundary: text,
    strongest_argument_against: text, remedy_tradeoffs: text,
    side_observations: text, scripts: text, surviving_claim: text, known: text,
  },
  required: ['id', 'verdict', 'reproduced', 'native_control', 'intended_evidence', 'contract_evidence', 'cause_reading', 'boundary', 'strongest_argument_against', 'remedy_tradeoffs', 'surviving_claim', 'known'],
}
const prompt = id =>
  'Read ' + S + '/brief-refuter.md completely and follow it exactly. The candidate is ' + S + '/cands/' + id + '.md (candidate id ' + id + '); its script path is in that file. ' +
  'Your verification directory is ' + S + '/verify/' + id + '/ (create it; an earlier attempt may have left scripts there, which you may ignore). ' +
  'This is ordinary functional-correctness review of an open-source R data-summary package: you check whether a reported behaviour is a real defect or intended. ' +
  'You know of no other candidate and need none. Write no report file. Return the structured result with id = "' + id + '".'
phase('Refute')
const results = await parallel(args.groups.map(g => async () => {
  for (let attempt = 1; attempt <= 2; attempt++) {
    const r = await agent(prompt(g.id), { label: 'refute ' + g.id + (attempt > 1 ? ' (second try)' : ''), phase: 'Refute', model: g.refuter, effort: 'high', schema: RETURN })
    if (r) { r.model_used = g.refuter; r.attempts = attempt; return r }
    log('refute ' + g.id + ': ' + g.refuter + ' returned nothing on attempt ' + attempt)
  }
  return { id: g.id, failed: true, model: g.refuter }
}))
return results
