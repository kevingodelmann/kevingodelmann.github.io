"""
Does the mushroom body actually learn a policy, and can it re-learn?

The v4 learning provably could not change behaviour (it scaled an already
saturated output). This checks the v5 claim directly and behaviourally: we
pick one context, declare one action "correct", reward it and punish
everything else, and watch what the fly chooses.

Then -- the part v4 could never have passed -- we CHANGE which action is
correct, to test that the homeostatic recovery term really does let it
re-learn instead of staying pinned at its first answer.
"""
import json
import urllib.request

BASE = "http://127.0.0.1:8723"  # NOT localhost: it resolves to ::1 first,
# httpuv binds IPv4 only, and every request then eats a ~2s fallback timeout.


def get(path):
    with urllib.request.urlopen(BASE + path, timeout=10) as r:
        return json.load(r)


def decide(tx, spin):
    return get(f"/decide?tx={tx}&ty=0&urgency=1&spin={spin}")


def sample_policy(tx, spin, n=60):
    """What does the fly actually choose, before any weights are touched?"""
    counts = {}
    for _ in range(n):
        d = decide(tx, spin)
        key = f"{d['stroke']}/{d['placement']}"
        counts[key] = counts.get(key, 0) + 1
    return dict(sorted(counts.items(), key=lambda kv: -kv[1]))


def train(tx, spin, target_action, rounds):
    """Reward the target action, punish everything else."""
    for _ in range(rounds):
        d = decide(tx, spin)
        won = 1 if d["action"] == target_action else 0
        get(f"/outcome?won={won}")


def top(counts):
    k = max(counts, key=counts.get)
    return k, counts[k]


TX, SPIN = 0.5, 0.0
ctx = decide(TX, SPIN)["ctx"]
print(f"context id = {ctx}\n")

before = sample_policy(TX, SPIN)
print("BEFORE training (expect ~uniform over 9 actions):")
print(" ", before)
print(f"  most common: {top(before)[0]} at {top(before)[1]}/60\n")

TARGET_A = 3
train(TX, SPIN, TARGET_A, 400)
after = sample_policy(TX, SPIN)
pol = get("/policy")
name_a = f"{pol['strokes'][TARGET_A - 1]}/{pol['placements'][TARGET_A - 1]}"
print(f"AFTER 400 trials rewarding action {TARGET_A} ({name_a}):")
print(" ", after)
print(f"  most common: {top(after)[0]} at {top(after)[1]}/60")
print(f"  target chosen {after.get(name_a, 0)}/60\n")

# Now move the goalposts. v4 could never pass this: with no decay term its
# weights were pinned at their floor permanently after the first few minutes.
TARGET_B = 7
train(TX, SPIN, TARGET_B, 400)
relearn = sample_policy(TX, SPIN)
name_b = f"{pol['strokes'][TARGET_B - 1]}/{pol['placements'][TARGET_B - 1]}"
print(f"AFTER re-training onto action {TARGET_B} ({name_b}):")
print(" ", relearn)
print(f"  most common: {top(relearn)[0]} at {top(relearn)[1]}/60")
print(f"  new target chosen {relearn.get(name_b, 0)}/60")
print(f"  old target chosen {relearn.get(name_a, 0)}/60\n")

# A context we never trained must be untouched -- otherwise "learning" is just
# a global drift and the sparse code is not separating contexts at all.
other = sample_policy(-0.9, 0.0)
print("UNTRAINED context (must stay ~uniform; proves contexts are separable):")
print(" ", other)
print(f"  most common: {top(other)[0]} at {top(other)[1]}/60")

print("\n--- verdict ---")
ok1 = after.get(name_a, 0) > 30
ok2 = relearn.get(name_b, 0) > 30
ok3 = top(other)[1] < 25
print(f"  learned initial target:     {'PASS' if ok1 else 'FAIL'}")
print(f"  re-learned after switch:    {'PASS' if ok2 else 'FAIL'}")
print(f"  untrained context unharmed: {'PASS' if ok3 else 'FAIL'}")
