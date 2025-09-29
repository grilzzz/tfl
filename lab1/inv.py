import random
import numpy as np
from collections import Counter

random.seed(12345)

Alphabet = ['a','b','c']


T = [
    ("abbc","cbba"),
    ("accb","bcca"),
    ("baac","caab"),
    ("cb","bc"),
    ("ca","ab"),
]

# генерируем новые термы по найденной закономерности
def abncc_rule(n: int):
    return ("a" + "b"*n + "cc", "b" + "a" + "b"*(n+1))

T_prime = T + [abncc_rule(n) for n in (1,2,3)]


def neighbors(word: str, rules):
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
    return list(outs)


def random_word(min_len=4, max_len=14):
    L = random.randint(min_len, max_len)
    return "".join(random.choice(Alphabet) for _ in range(L))

def random_chain(word: str, rules, min_steps=1, max_steps=8):
    w = word
    chain = [w]
    steps = random.randint(min_steps, max_steps)
    for _ in range(steps):
        ns = neighbors(w, rules)
        if not ns:
            break
        w = random.choice(ns)
        chain.append(w)
    return chain


def counts(w: str):
    c = Counter(w)
    return c.get('a', 0), c.get('b', 0), c.get('c', 0)

def monomial_exponents(w: str):
    a, b, c = counts(w)
    return (a, b + c)


def _rand_diag(dim=2, low=0.5, high=2.0, rng=None):
    """
    Сгенерировать случайную диагональную матрицу
    """
    if rng is None:
        rng = np.random.default_rng()
    d = rng.uniform(low, high, size=dim)
    return np.diag(d)

def matrix_invariant(
    w: str,
    A: np.ndarray | None = None,
    X: np.ndarray | None = None,
    *,
    dim: int = 2,
    rng_seed: int | None = None,
    low: float = 0.5,
    high: float = 2.0,
    return_matrices: bool = False,
) -> np.ndarray | tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Матричный инвариант, умножая матрицы В ПОРЯДКЕ букв слова:
        M_a = A, M_b = M_c = X.

    Перемножение этих патриц в порядке следования букв является инвариантом.
    В итоговой проверке используются след и определитель матриц, потому что сами матрицы проверять на равенство мне лень. 
    """
    
    rng = np.random.default_rng(rng_seed) if (A is None or X is None) else None

    if A is None:
        A = _rand_diag(dim=dim, low=low, high=high, rng=rng)
    if X is None:
        X = _rand_diag(dim=dim, low=low, high=high, rng=rng)

    if A.shape != X.shape or A.shape[0] != A.shape[1]:
        raise ValueError("A и X должны быть квадратными и одинакового размера.")
        
    if not np.allclose(A @ X, X @ A):
        raise ValueError("A и X обязаны коммутировать: A@X == X@A.")
    
    n = A.shape[0]
    M = np.eye(n)
    for ch in w:
        if ch == 'a':
            M = M @ A
        elif ch == 'b' or ch == 'c':
            M = M @ X

    return (M, A, X) if return_matrices else M



def matrix_invariant_trace(w: str, **kwargs) -> float:
    """След матрицы-инварианта tr(M(w))."""
    return round(float(np.trace(matrix_invariant(w, **kwargs))), 2)

def matrix_invariant_det(w: str, **kwargs) -> float:
    """Определитель det(M(w))."""
    import numpy as _np
    return round(float(_np.linalg.det(matrix_invariant(w, **kwargs))), 2)


def parity_b_minus_c(w: str):
    """ (|b| - |c|) mod 2 """
    _, b, c = counts(w)
    return (b - c) % 2

def block_parity_total(w: str) -> int:
    """
    Инвариант: чётность числа x-блоков нечётной длины после проекции b,c -> 'x' и сохранения 'a'.
    Возвращает 0 или 1.
    """
    proj = ['x' if ch in ('b', 'c') else 'a' for ch in w]
    n = len(proj)
    i = 0
    odd_total = 0
    while i < n:
        while i < n and proj[i] == 'a':
            i += 1
        if i >= n:
            break
        xlen = 0
        while i < n and proj[i] == 'x':
            xlen += 1
            i += 1
        if xlen & 1:
            odd_total ^= 1
    return odd_total


A=_rand_diag()
X=_rand_diag()

def invariants(w: str):
    a, b, c = counts(w)
    inv = {
        "len": len(w),
        "cnt_a": a,
        "sum_bc": b + c,
        "parity_b_minus_c": parity_b_minus_c(w),
        "monomial_exponents": monomial_exponents(w), 
        "matrix_invariant_trace": matrix_invariant_trace(w, A=A, X=X),
        "matrix_invariant_det": matrix_invariant_det(w, A=A, X=X),
        "block_parity_total": block_parity_total(w),
    }
    return inv


def check_chain(chain):
    base = invariants(chain[0])
    for w in chain[1:]:
        inv = invariants(w)
        if inv != base:
            return False, base, inv, w
    return True, base, base, None


def unit_check_rules(rules):
    ok = True
    bad = []
    for lhs, rhs in rules:
        if invariants(lhs) != invariants(rhs):
            ok = False
            bad.append((lhs, rhs, invariants(lhs), invariants(rhs)))
    return ok, bad


def fuzz_test(rules_A, rules_B, trials=200, min_len=4, max_len=14, min_steps=1, max_steps=8, show=5):
    report = []
    okA = okB = 0
    for t in range(trials):
        w = random_word(min_len, max_len)
        chain_A = random_chain(w, rules_A, min_steps, max_steps)
        goodA, baseA, invA, wbadA = check_chain(chain_A)
        if goodA:
            okA += 1
        else:
            report.append(("A_FAIL", w, chain_A, baseA, invA, wbadA))
        chain_B = random_chain(w, rules_B, min_steps, max_steps)
        goodB, baseB, invB, wbadB = check_chain(chain_B)
        if goodB:
            okB += 1
        else:
            report.append(("B_FAIL", w, chain_B, baseB, invB, wbadB))
    summary = {
        "trials": trials,
        "A_ok": okA,
        "B_ok": okB,
        "A_fail": trials - okA,
        "B_fail": trials - okB,
    }
    
    examples = []
    for kind, w0, chain, base, inv_bad, wbad in report[:show]:
        examples.append({
            "kind": kind,
            "w0": w0,
            "chain": chain,
            "violation_at": wbad,
            "base_invariants": base,
            "bad_invariants": inv_bad,
        })
    return summary, examples


ok_T, bad_T = unit_check_rules(T)
ok_Tp, bad_Tp = unit_check_rules(T_prime)

print("Unit check: each rule preserves invariants")
print(f"Original T:  {'OK' if ok_T else 'FAIL'}")
if not ok_T:
    for (l,r,il,ir) in bad_T:
        print("  Rule FAIL:", l, "<->", r, "inv(lhs)=", il, "inv(rhs)=", ir)
print(f"Extended T': {'OK' if ok_Tp else 'FAIL'}")
if not ok_Tp:
    for (l,r,il,ir) in bad_Tp:
        print("  Rule FAIL:", l, "<->", r, "inv(lhs)=", il, "inv(rhs)=", ir)

print("\nFuzz tests on random chains (both directions allowed)")
sum_AB, examples = fuzz_test(T, T_prime, trials=3000, min_len=10, max_len=30, min_steps=1, max_steps=15, show=5)
print(sum_AB)
if examples:
    print("\nSample violations:")
    for ex in examples:
        print(ex)
