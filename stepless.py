from __future__ import print_function

from collections import defaultdict
import itertools
import sys

import clingo

def make_sym_map(iteratoms):
    result = defaultdict(list)
    for s in iteratoms:
        result[s.name].append(s.arguments)
    return result

def toposort(vertices, edges):
    visited = set()
    def sortFrom(vertex):
        if vertex in visited:
            return
        visited.add(vertex)
        for e in edges[vertex]:
            for x in sortFrom(e):
                yield x
        yield vertex
    result = list(itertools.chain(*(map(sortFrom,vertices))))
    result.reverse()
    return result

def print_action_plan(symbols,file):
    vertices = set(v for [v] in symbols['vertex'])
    eventToVertex = {k:v for [k,v] in symbols['inVertex']}
    edges = defaultdict(list)
    for [vfrom,vto] in symbols['edge']:
        edges[eventToVertex[vfrom]].append(eventToVertex[vto])
    verts = toposort(vertices, edges)
    actions = [v for v in verts if v.name == 'actOcc']
    for a in actions:
        print(a.arguments[0],file=file)

def print_suffix_plan(symbols,file):
    actions = {v for [v] in symbols['happens']}
    vertices = actions.union(set(v for [v] in symbols['holds']))
    edges = defaultdict(list)
    for [vfrom,vto] in symbols['causes'] + symbols['permits']:
        edges[vfrom].append(vto)
    verts = toposort(vertices,edges)
    actions = [v for v in verts if v in actions]
    for v in verts:
        if v in actions:
            print('suffix',v,file=file)

def print_model_plan(m, file):
    symbols = make_sym_map(m.symbols(shown=True))
    print_action_plan(symbols,file)
    suffix_symbols = make_sym_map(x for [x] in symbols['suffix'])
    print_suffix_plan(suffix_symbols,file)
    print('cost:',m.cost[0])

def new_occs(m):
    for s in m.symbols(atoms=True):
        if s.name == 'saturated':
            arg = s.arguments[0]
            if arg.name == 'action':
                r = ('actOcc', arg.arguments[0])
                yield r
            elif arg.name == 'fluent':
                r = ('fluentOcc', arg.arguments[0])
                yield r

def rule_of(k0,v):
    (t,k) = k0
    return str(clingo.Function('is', [clingo.Function(t,[k,v])])) + '.'

def main(*infiles):
    occs = defaultdict(lambda: 1)
    while True:
        c = clingo.Control(['--opt-strategy=usc'])
        for f in infiles:
            c.load(f)
        c.load("asp/stepless.asp")
        parts = [(t,[k,i]) for ((t,k),v) in occs.iteritems() for i in range(2,v+1)]
        c.ground([("base",[])] + parts)
        with c.solve(yield_=True) as handle:
            for m in handle:
                pass
            res = handle.get()
            if res.interrupted:
                break
            elif res.satisfiable:
                if not m.contains(clingo.Function('useSuffix',[])):
                    print_model_plan(m, sys.stdout)
                    break
                print_model_plan(m, sys.stderr)
                print(file=sys.stderr)
                print('Adding:',file=sys.stderr)
                for k in new_occs(m):
                    v = occs[k] + 1
                    occs[k] = v
                    print(rule_of(k,v),file=sys.stderr)
                print(file=sys.stderr)
                print(file=sys.stderr)
            else:
                print('no solution')
                break


if __name__ == '__main__':
    main(*sys.argv[1:])
