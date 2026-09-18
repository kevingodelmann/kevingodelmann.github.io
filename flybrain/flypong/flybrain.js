/**
 * Bridge to the malecns connectome server (fly_server_v5.R on :8723).
 *
 * Two channels, and it matters which is which:
 *
 *   STEERING  is computed by the connectome. The ball's position is sent as a
 *   graded visual coordinate, every LC10 neuron is driven by a Gaussian on the
 *   distance from its own receptive field to that target, and the real synaptic
 *   weights carry it to the DNs. Direction falls out of the difference between
 *   the two eyes. Nothing about the sign or the magnitude is hand-written here.
 *
 *   ACTION SELECTION -- which stroke to play and where to aim it -- is chosen
 *   by the mushroom body, and is what the dopamine learning actually changes.
 *   In v4 learning multiplied a saturated number and provably could not affect
 *   the game; now it picks between nine real options.
 *
 * Height tracking remains a plain assist and is labelled as such below.
 */

export function createFlyBrain(host = location.hostname) {
  // "localhost" resolves to ::1 first, but httpuv binds IPv4 only, so every
  // single request pays a ~2s connection-fallback timeout before succeeding.
  // Measured: 2.05s per request via localhost vs 0.006s via 127.0.0.1 -- a
  // 350x difference that made the brain look far slower than it is.
  const resolved = host === 'localhost' ? '127.0.0.1' : host;
  const server = `http://${resolved}:8723`;

  const fly = {
    enabled: false,
    x: 0,                 // lateral drive from the pursuit circuit, in [-1,1]
    stroke: 'drive',      // chosen by the mushroom body
    placement: 'centre',
    value: 0,             // MBON readout: approach drive minus avoidance drive
    ctx: null,
    pendingOutcome: false,
    busy: false,
    failures: 0,
    lastError: null,
  };

  /**
   * Visual coordinates. The circuit expects a normalised visual field, so the
   * table's half-width maps to the eyes' azimuth range rather than to metres.
   */
  function toVisual(ballX, paddleX, ballY) {
    const dx = ballX - paddleX;
    return {
      tx: Math.max(-1, Math.min(1, dx / 0.75)),
      ty: Math.max(-1, Math.min(1, ((ballY ?? 0.95) - 0.95) / 0.35)),
    };
  }

  /** Steering only: called every frame during a rally. */
  async function poll(ballX, paddleX, ballY) {
    if (!fly.enabled || fly.busy) return;
    const { tx, ty } = toVisual(ballX, paddleX, ballY);
    if (!Number.isFinite(tx) || !Number.isFinite(ty)) return;

    fly.busy = true;
    try {
      const res = await fetch(`${server}/move?gx=${tx.toFixed(3)}&gy=${ty.toFixed(3)}&gurgency=1`);
      const j = await res.json();
      fly.x = Number.isFinite(j.x) ? j.x : 0;
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

  /**
   * One decision per incoming shot: the mushroom body picks the stroke and the
   * placement for this context. `spin` is the incoming ball's topspin sign,
   * which together with its lateral position forms the context the KCs encode.
   */
  async function decide(ballX, paddleX, ballY, spin) {
    if (!fly.enabled) return;
    const { tx, ty } = toVisual(ballX, paddleX, ballY);
    if (!Number.isFinite(tx) || !Number.isFinite(ty)) return;
    const s = Math.max(-1, Math.min(1, (spin || 0) / 400));
    try {
      const res = await fetch(
        `${server}/decide?tx=${tx.toFixed(3)}&ty=${ty.toFixed(3)}&urgency=1&spin=${s.toFixed(3)}`,
      );
      const j = await res.json();
      if (Number.isFinite(j.x)) fly.x = j.x;
      fly.stroke = j.stroke ?? 'drive';
      fly.placement = j.placement ?? 'centre';
      fly.value = j.value ?? 0;
      fly.ctx = j.ctx ?? null;
      fly.pendingOutcome = true;
    } catch (err) {
      fly.lastError = err;
    }
  }

  /**
   * Winning or losing the point is the dopamine signal. Reward depresses the
   * avoidance channel for the action just taken; punishment depresses the
   * approach channel. Both are depression, which is the real rule.
   *
   * Only fires if a decision is actually outstanding -- otherwise a point that
   * ended without the fly striking the ball would reinforce a stale choice.
   */
  function outcome(won, strength = 1) {
    if (!fly.enabled || !fly.pendingOutcome) return;
    fetch(`${server}/outcome?won=${won ? 1 : 0}&strength=${strength}`).catch(() => {});
  }

  /** Called when a new shot is coming, so the previous decision stops being
   *  reinforceable and a stale choice can't absorb the next point's outcome. */
  function clearPending() {
    fly.pendingOutcome = false;
  }

  return { fly, poll, decide, outcome, clearPending, server };
}
