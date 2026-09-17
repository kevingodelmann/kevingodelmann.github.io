/**
 * Bridge to the malecns connectome server (fly_server_v4.R on :8721).
 *
 * The contract is deliberately unchanged from the previous version so the
 * existing dashboard keeps working: lateral steering comes from the pursuit
 * circuit (LC10 -> DNa10 -> VNC interneurons -> motor neurons), and winning a
 * point fires the appetitive dopamine reward that potentiates it.
 *
 * Only the x channel is routed through the circuit. That is the one output
 * whose sign convention has been verified end to end; the depth channel is
 * handled by a plain heuristic in the game layer and is labelled as such.
 */

export function createFlyBrain(host = location.hostname) {
  const server = `http://${host}:8721`;

  const fly = {
    enabled: false,
    x: 0,
    potentiation: 0,
    busy: false,
    failures: 0,
    lastError: null,
  };

  async function poll(ballX, paddleX) {
    if (!fly.enabled || fly.busy) return;
    const dx = ballX - paddleX;
    const gx = dx / Math.max(0.05, Math.abs(dx));
    if (!Number.isFinite(gx)) return;

    fly.busy = true;
    try {
      const res = await fetch(
        `${server}/move?ex=0&ey=0&urgency=0&gx=${gx.toFixed(3)}&gy=0&gurgency=1`,
      );
      const j = await res.json();
      fly.x = Number.isFinite(j.x) ? j.x : 0;
      fly.potentiation = j.potentiation ?? 0;
      fly.failures = 0;
      fly.lastError = null;
    } catch (err) {
      fly.failures += 1;
      fly.lastError = err;
      if (fly.failures > 5) fly.enabled = false;
    } finally {
      fly.busy = false;
    }
  }

  /** Winning a point is this game's equivalent of Nightfall's XP pickup. */
  function reward() {
    if (!fly.enabled) return;
    fetch(`${server}/reward`).catch(() => {});
  }

  return { fly, poll, reward, server };
}
