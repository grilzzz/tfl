import random
from collections import deque, defaultdict

random.seed(42)

Alphabet = ['a','b','c']

T = [
    ("abbc","cbba"),
    ("accb","bcca"),
    ("baac","caab"),
    ("cb","bc"),
    ("ca","ab"),
]

# генерируем новые термы по найденной закономерности
def abncc_rule(n):
    return ("a" + "b"*n + "cc", "b" + "a" + "b"*(n+1))

N_extra = [abncc_rule(n) for n in [1,2,3]]
T_prime = T + N_extra

# не используется
def neighbors(word, rules):
    outs = set()
    for lhs, rhs in rules:
        # forward
        start = 0
        while True:
            i = word.find(lhs, start)
            if i == -1:
                break
            outs.add(word[:i] + rhs + word[i+len(lhs):])
            start = i + 1
        # backward
        start = 0
        while True:
            i = word.find(rhs, start)
            if i == -1:
                break
            outs.add(word[:i] + lhs + word[i+len(rhs):])
            start = i + 1
    outs.discard(word)
    return outs

def neighbors_fwd(word: str, rules):
    """Все одношаговые подстановки только вперёд (lhs -> rhs)."""
    outs = set()
    for lhs, rhs in rules:
        start = 0
        while True:
            i = word.find(lhs, start)
            if i == -1:
                break
            outs.add(word[:i] + rhs + word[i+len(lhs):])
            start = i + 1
    outs.discard(word)
    return outs


def rand_word(min_len=10, max_len=20):
    L = random.randint(min_len, max_len)
    return "".join(random.choice(Alphabet) for _ in range(L))


def random_chain(word, rules, min_steps=1, max_steps=20):
    w = word
    chain = [w]
    steps = random.randint(min_steps, max_steps)
    for _ in range(steps):
        neigh = list(neighbors_fwd(w, rules))
        if not neigh:
            break
        w = random.choice(neigh)
        chain.append(w)
    return chain


def reachable_unidirectional(src: str, dst: str, rules, max_depth=40, max_nodes=500000):

    if src == dst:
        return True, [src]

    q = deque([src])
    parents = {src: None}
    depth = {src: 0}
    visited = {src}
    expanded = 0

    while q and expanded < max_nodes:
        u = q.popleft()
        d = depth[u]
        if d >= max_depth:
            continue

        for v in neighbors_fwd(u, rules):
            expanded += 1
            if v in visited:
                continue
            visited.add(v)
            parents[v] = u
            depth[v] = d + 1
            if v == dst:
                path = []
                x = v
                while x is not None:
                    path.append(x)
                    x = parents[x]
                path.reverse()
                return True, path
            q.append(v)

    return False, []

# не используется (сначала делал так чтобы можно было ходить в 2 стороны)
def reachable_bidirectional(src, dst, rules, max_depth=40, max_nodes=500000):
    if src == dst:
        return True, [src]
    frontL = {src}
    frontR = {dst}
    parentsL = {src: None}
    parentsR = {dst: None}
    depth = 0
    visitedL = {src}
    visitedR = {dst}
    expanded = 0
    while frontL and frontR and depth < max_depth and expanded < max_nodes:
        if len(frontL) <= len(frontR):
            new_front = set()
            for u in frontL:
                for v in neighbors(u, rules):
                    expanded += 1
                    if v in visitedL:
                        continue
                    visitedL.add(v)
                    parentsL[v] = u
                    if v in visitedR:
                        pathL = []
                        x = v
                        while x is not None:
                            pathL.append(x)
                            x = parentsL[x]
                        pathL = pathL[::-1] 
                        
                        pathR = []
                        x = v
                        while x is not None:
                            pathR.append(x)
                            x = parentsR[x]
                        pathR = pathR[1:]  
                        full = pathL + pathR
                        return True, full
                    new_front.add(v)
                    if expanded >= max_nodes:
                        break
                if expanded >= max_nodes:
                    break
            frontL = new_front
        else:
            new_front = set()
            for u in frontR:
                for v in neighbors(u, rules):
                    expanded += 1
                    if v in visitedR:
                        continue
                    visitedR.add(v)
                    parentsR[v] = u
                    if v in visitedL:
                        pathL = []
                        x = v
                        while x is not None:
                            pathL.append(x)
                            x = parentsL[x]
                        pathL = pathL[::-1]
                        pathR = []
                        x = v
                        while x is not None:
                            pathR.append(x)
                            x = parentsR[x]
                        pathR = pathR[1:]
                        full = pathL + pathR
                        return True, full
                    new_front.add(v)
                    if expanded >= max_nodes:
                        break
                if expanded >= max_nodes:
                    break
            frontR = new_front
        depth += 1
    return False, []


def fuzz(num_trials=3000, show_examples=10):
    successes = 0
    failures = 0
    examples = []
    for t in range(num_trials):
        w0 = rand_word(min_len=10, max_len=30)
        chain = random_chain(w0, T, 1, 10)
        w1 = chain[-1]
        ok, path = reachable_unidirectional(w0, w1, T_prime, max_depth=35, max_nodes=1000000)
        if ok:
            successes += 1
        else:
            failures += 1
            if len(examples) < show_examples:
                examples.append(("FAIL", w0, w1, chain, path))

    return successes, failures, examples

successes, failures, examples = fuzz()

print(f"Trials: {successes+failures}, Successes: {successes}, Failures: {failures}")
# print("\nSample cases:")
for status, w0, w1, chain, path in examples:
    # print(status)
    # print(f"w:  {w0}")
    # print(f"w′: {w1}")
    # print(f"Random chain by T (w =>* w′): { ' => '.join(chain) }")
    pass