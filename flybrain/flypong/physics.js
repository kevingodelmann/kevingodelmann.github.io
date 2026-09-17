/**
 * Table tennis ball physics.
 *
 * Deliberately free of any rendering dependency so it can be reasoned about
 * and tested on its own. Everything here is SI units (metres, seconds, kg,
 * rad/s) and uses real measured constants for a 40mm competition ball rather
 * than hand-tuned magic numbers.
 *
 * The three things that make a ping-pong ball behave like a ping-pong ball,
 * none of which the previous implementation had:
 *
 *  1. Drag. The ball is 2.7g with a 40mm cross-section, so air resistance is
 *     the same order of magnitude as gravity at rally speeds — it is not a
 *     rounding error, it visibly shortens and steepens every trajectory.
 *  2. Magnus force. Topspin pushes the ball DOWN, which is the entire reason
 *     players can hit hard and still land it on a 2.7m table. Backspin floats.
 *     Sidespin curves.
 *  3. Spin-coupled bounces. Friction at the contact patch trades spin for
 *     tangential speed, so topspin kicks forward off the bounce and backspin
 *     skids, stops, or even comes back.
 */

export const BALL_RADIUS = 0.020;      // m   (40mm ITTF ball)
export const BALL_MASS = 0.0027;       // kg  (2.7g ITTF ball)

const AIR_DENSITY = 1.204;             // kg/m^3 at 20C
const DRAG_COEFF = 0.40;               // sphere at rally Reynolds numbers
// Lift coefficient vs spin ratio S = r*omega/v. A hard clamp here was wrong:
// it saturated by S~0.5, so 25 rev/s and 80 rev/s of topspin produced
// identical trajectories and a loop felt no different from a drive. Real
// measurements rise steeply at low S and roll off gradually, so use a smooth
// saturating curve that keeps responding across the whole playable range.
const LIFT_COEFF_MAX = 0.50;
const LIFT_HALF_SATURATION = 0.85;     // S at which Cl reaches half of its ceiling
const CROSS_SECTION = Math.PI * BALL_RADIUS * BALL_RADIUS;

export const GRAVITY = 9.81;

// a_drag = -(rho*Cd*A / 2m) * |v| * v
const K_DRAG = (AIR_DENSITY * DRAG_COEFF * CROSS_SECTION) / (2 * BALL_MASS);
// a_lift = (rho*Cl*A / 2m) * |v|^2 * (spinAxis x velocityDir)
const K_LIFT = (AIR_DENSITY * CROSS_SECTION) / (2 * BALL_MASS);

// Spin bleeds off in flight, slowly. Time constant of a few seconds.
const SPIN_DECAY_PER_SEC = 0.12;

/** Official table dimensions — single source of truth for physics and rendering. */
export const TABLE = {
  length: 2.74,
  width: 1.525,
  height: 0.76,
  netHeight: 0.1525,
  netOverhang: 0.1525,   // net extends this far past each side edge
};

export const HALF_LENGTH = TABLE.length / 2;
export const HALF_WIDTH = TABLE.width / 2;
export const TOP_Y = TABLE.height;              // playing surface height
const REST_Y = TABLE.height + BALL_RADIUS;      // ball centre when resting on the table

/* ------------------------------ vector helpers ----------------------------- */
export const vec = (x = 0, y = 0, z = 0) => ({ x, y, z });
const add = (a, b) => vec(a.x + b.x, a.y + b.y, a.z + b.z);
const sub = (a, b) => vec(a.x - b.x, a.y - b.y, a.z - b.z);
const scale = (a, s) => vec(a.x * s, a.y * s, a.z * s);
const dot = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;
const cross = (a, b) => vec(
  a.y * b.z - a.z * b.y,
  a.z * b.x - a.x * b.z,
  a.x * b.y - a.y * b.x,
);
const len = (a) => Math.hypot(a.x, a.y, a.z);
const norm = (a) => { const l = len(a); return l > 1e-9 ? scale(a, 1 / l) : vec(); };

export { add, sub, scale, dot, cross, len, norm };

/* -------------------------------- ball state ------------------------------- */
export function makeBall() {
  return { pos: vec(0, REST_Y + 0.3, 0), vel: vec(), spin: vec() };
}

/**
 * Acceleration on the ball: gravity + quadratic drag + Magnus lift.
 *
 * The lift coefficient depends on the spin ratio S = r*omega/v rather than
 * being constant, and saturates — otherwise absurd spin would produce absurd
 * force. This is the standard empirical form used in table tennis literature.
 */
export function acceleration(vel, spin) {
  const speed = len(vel);
  const a = vec(0, -GRAVITY, 0);
  if (speed < 1e-6) return a;

  // drag, opposing motion, proportional to v^2
  const dragMag = K_DRAG * speed;
  a.x -= dragMag * vel.x;
  a.y -= dragMag * vel.y;
  a.z -= dragMag * vel.z;

  // Magnus: only the spin component perpendicular to travel generates lift
  const spinMag = len(spin);
  if (spinMag > 1e-6) {
    const axis = norm(spin);
    const dir = norm(vel);
    const perp = cross(axis, dir);          // |perp| = sin(angle between them)
    const perpMag = len(perp);
    if (perpMag > 1e-6) {
      const spinRatio = (BALL_RADIUS * spinMag * perpMag) / speed;
      const cl = (LIFT_COEFF_MAX * spinRatio) / (spinRatio + LIFT_HALF_SATURATION);
      const liftMag = K_LIFT * cl * speed * speed;
      const liftDir = scale(perp, 1 / perpMag);
      a.x += liftMag * liftDir.x;
      a.y += liftMag * liftDir.y;
      a.z += liftMag * liftDir.z;
    }
  }
  return a;
}

/**
 * Rigid-sphere impulse against a surface with outward normal `n`.
 *
 * Friction either brings the contact patch to rest (the ball grips and starts
 * rolling) or saturates at the Coulomb limit (the ball skids). This single
 * routine is what produces the topspin-kicks-forward / backspin-skids-back
 * behaviour, for the table and for the paddle alike.
 *
 * Mutates and returns { vel, spin }.
 */
function applyContactImpulse(vel, spin, n, restitution, friction) {
  const vn = dot(vel, n);
  if (vn > 0) return { vel, spin };   // already separating

  // normal impulse reverses the approach speed, scaled by restitution
  const jn = -(1 + restitution) * vn;                    // per unit mass
  let newVel = add(vel, scale(n, jn));

  // velocity of the material point touching the surface
  const contactArm = scale(n, -BALL_RADIUS);
  const contactVel = add(vel, cross(spin, contactArm));
  const tangentVel = sub(contactVel, scale(n, dot(contactVel, n)));
  const tangentMag = len(tangentVel);

  if (tangentMag > 1e-6) {
    // impulse that would exactly stop the contact patch (2/7 for a solid sphere)
    const gripImpulse = (2 / 7) * tangentMag;
    const slipLimit = friction * jn;
    const jt = -Math.min(gripImpulse, slipLimit);
    const tangentDir = scale(tangentVel, 1 / tangentMag);
    const impulse = scale(tangentDir, jt);

    newVel = add(newVel, impulse);
    // dOmega = (r x J) / I, with I = (2/5) m r^2
    const inertia = (2 / 5) * BALL_RADIUS * BALL_RADIUS;
    spin = add(spin, scale(cross(contactArm, impulse), 1 / inertia));
  }
  return { vel: newVel, spin };
}

const TABLE_RESTITUTION = 0.86;   // measured ITTF ball-on-table rebound
// Ball-on-table friction. At 0.25 the tangential impulse saturated on every
// impact, so a no-spin ball and a heavy backspin ball came off the bounce
// identically — spin could not express itself at all. 0.35 sits in the middle
// of the measured range and lets a moderate ball grip and roll rather than
// always skidding.
const TABLE_FRICTION = 0.35;
const PADDLE_RESTITUTION = 0.80;
const PADDLE_FRICTION = 0.85;     // inverted rubber grips hard — this makes spin

export function bounceOffTable(vel, spin) {
  return applyContactImpulse(vel, spin, vec(0, 1, 0), TABLE_RESTITUTION, TABLE_FRICTION);
}

/**
 * Ball meets paddle. The paddle carries its own velocity, so we work in the
 * paddle's frame: subtract paddle velocity, apply the contact impulse against
 * the paddle's face normal, then add it back.
 *
 * A paddle brushing upward across the back of the ball produces topspin here
 * as an emergent consequence of friction, not as a special case.
 */
export function bounceOffPaddle(vel, spin, paddleNormal, paddleVel) {
  const rel = sub(vel, paddleVel);
  const out = applyContactImpulse(rel, spin, norm(paddleNormal), PADDLE_RESTITUTION, PADDLE_FRICTION);
  return { vel: add(out.vel, paddleVel), spin: out.spin };
}

/* ------------------------------- integration ------------------------------- */

const SUBSTEPS = 4;   // enough that a 10 m/s ball can't tunnel the 4cm-thick contact zone

function overTable(pos) {
  return Math.abs(pos.x) <= HALF_WIDTH && Math.abs(pos.z) <= HALF_LENGTH;
}

/**
 * Advance the ball by dt, resolving collisions by detecting plane CROSSINGS
 * between substeps rather than testing whether the ball happens to be inside
 * a height band on a given frame. The old band test both missed fast balls and
 * "bounced" them off an invisible infinite plane well past the table's edge.
 *
 * Returns the list of events that occurred, for the rules layer to interpret.
 */
export function step(ball, dt) {
  const events = [];
  const h = dt / SUBSTEPS;

  for (let i = 0; i < SUBSTEPS; i++) {
    const prev = { ...ball.pos };
    const a = acceleration(ball.vel, ball.spin);

    ball.vel = add(ball.vel, scale(a, h));
    ball.pos = add(ball.pos, scale(ball.vel, h));
    ball.spin = scale(ball.spin, Math.max(0, 1 - SPIN_DECAY_PER_SEC * h));

    // --- net: a vertical plane at z = 0, spanning slightly wider than the table
    if (Math.sign(prev.z) !== Math.sign(ball.pos.z) && prev.z !== 0) {
      const t = prev.z / (prev.z - ball.pos.z);
      const crossY = prev.y + (ball.pos.y - prev.y) * t;
      const crossX = prev.x + (ball.pos.x - prev.x) * t;
      const withinNetSpan = Math.abs(crossX) <= HALF_WIDTH + TABLE.netOverhang;
      if (withinNetSpan && crossY < TOP_Y + TABLE.netHeight + BALL_RADIUS && crossY > TOP_Y - 0.05) {
        events.push({ type: 'net', from: Math.sign(prev.z), x: crossX, y: crossY });
        // clip the net: kill most forward speed, drop it
        ball.pos = vec(crossX, crossY, prev.z > 0 ? 0.01 : -0.01);
        ball.vel = vec(ball.vel.x * 0.2, ball.vel.y * 0.2, -ball.vel.z * 0.12);
        ball.spin = scale(ball.spin, 0.3);
        break;
      }
    }

    // --- table top: only where the table actually is
    if (prev.y >= REST_Y && ball.pos.y < REST_Y && overTable(ball.pos)) {
      const t = (prev.y - REST_Y) / (prev.y - ball.pos.y);
      const hitX = prev.x + (ball.pos.x - prev.x) * t;
      const hitZ = prev.z + (ball.pos.z - prev.z) * t;
      ball.pos = vec(hitX, REST_Y, hitZ);
      const r = bounceOffTable(ball.vel, ball.spin);
      ball.vel = r.vel; ball.spin = r.spin;
      events.push({ type: 'bounce', side: Math.sign(hitZ) || 1, x: hitX, z: hitZ });
    }

    // --- floor: the rally is over
    if (ball.pos.y <= BALL_RADIUS) {
      ball.pos.y = BALL_RADIUS;
      events.push({ type: 'floor', x: ball.pos.x, z: ball.pos.z });
      ball.vel = vec(); ball.spin = vec();
      break;
    }
  }
  return events;
}

/* ----------------------------- shot generation ----------------------------- */

/**
 * Pick the launch elevation that lands a shot on a chosen spot.
 *
 * With drag and Magnus in play there is no closed-form answer, so we shoot and
 * correct: integrate the real forward model for a candidate elevation, see
 * where it first touches down, and bisect. A dozen iterations of a cheap
 * integration is nothing, and it means the aiming respects the same physics the
 * ball will actually fly under — including the fact that heavy topspin lets you
 * hit far harder and still land it, which is the whole point of the stroke.
 */
/** Fly a candidate launch and report what actually happened to it. */
function trace(from, dir, speed, elevation, spinVec, maxSteps = 700) {
  const c = Math.cos(elevation), s = Math.sin(elevation);
  const probe = {
    pos: { ...from },
    vel: vec(dir.x * speed * c, speed * s, dir.z * speed * c),
    spin: { ...spinVec },
  };
  const bounces = [];
  for (let i = 0; i < maxSteps; i++) {
    const evts = step(probe, 1 / 240);
    for (const e of evts) {
      if (e.type === 'net') return { outcome: 'net', bounces };
      if (e.type === 'floor') return { outcome: bounces.length ? 'done' : 'off', bounces };
      if (e.type === 'bounce') {
        bounces.push(e);
        if (bounces.length >= 2) return { outcome: 'done', bounces };
      }
    }
  }
  return { outcome: 'stalled', bounces };
}

export function solveLaunch(from, target, speed, spinVec) {
  const flat = vec(target.x - from.x, 0, target.z - from.z);
  const horizDist = len(flat);
  if (horizDist < 1e-4) return { vel: vec(0, speed, 0), spin: { ...spinVec } };
  const dir = scale(flat, 1 / horizDist);

  // Bisect on elevation. "Hit the net" and "flew too far" both used to be
  // lumped together as a single failure and corrected the same way, which is
  // backwards for one of them: netting means aim HIGHER, overshooting means
  // aim lower. Conflating them was why shots kept burying into the net.
  let lo = -0.2, hi = 1.1, best = 0.3;
  for (let i = 0; i < 18; i++) {
    const mid = (lo + hi) / 2;
    const r = trace(from, dir, speed, mid, spinVec);
    best = mid;
    if (r.outcome === 'net') { lo = mid; continue; }          // too flat — lift it
    if (r.outcome === 'off' || r.outcome === 'stalled') { hi = mid; continue; }
    const reach = len(vec(r.bounces[0].x - from.x, 0, r.bounces[0].z - from.z));
    if (reach < horizDist) lo = mid; else hi = mid;
  }

  const c = Math.cos(best), s = Math.sin(best);
  return { vel: vec(dir.x * speed * c, speed * s, dir.z * speed * c), spin: { ...spinVec } };
}

/**
 * A legal serve has to satisfy three constraints at once: bounce on the
 * server's own half, clear the net, then land on the receiver's half. There is
 * no single parameter that guarantees all three, so search over launch speed
 * and elevation, fly each candidate under the real model, and keep the one
 * that lands deepest on the far side.
 */
export function solveServe(from, dirSign, spinVec) {
  const dir = vec(0, 0, dirSign);
  let best = null;

  for (const speed of [3.8, 4.3, 4.8, 5.4, 6.0]) {
    for (let k = 0; k <= 20; k++) {
      const elevation = 0.02 + (k / 20) * 0.62;
      const r = trace(from, dir, speed, elevation, spinVec);
      if (r.outcome === 'net' || r.bounces.length < 2) continue;

      const first = r.bounces[0], second = r.bounces[1];
      // The first bounce must be on the SERVER's half — the side the ball was
      // struck from — and only the second on the receiver's. Accepting either
      // side here made the constraint vacuous and let serves sail straight
      // over the net without ever touching the server's half.
      const ownHalf = Math.sign(first.z) === Math.sign(from.z);
      const farHalf = Math.sign(second.z) === dirSign;
      if (!ownHalf || !farHalf) continue;

      // prefer a serve that lands well into the far half rather than dribbling
      // just over the net
      const depth = Math.abs(second.z) / HALF_LENGTH;
      const score = -Math.abs(depth - 0.62);
      if (!best || score > best.score) best = { speed, elevation, score };
    }
  }

  if (!best) {   // fall back to a safe lofted serve rather than failing outright
    best = { speed: 4.3, elevation: 0.38 };
  }
  const c = Math.cos(best.elevation), s = Math.sin(best.elevation);
  return {
    vel: vec(0, best.speed * s, dirSign * best.speed * c),
    spin: { ...spinVec },
  };
}

/**
 * Spin vector for a given amount of topspin (positive) or backspin (negative)
 * about the axis perpendicular to the direction of travel, plus optional
 * sidespin about the vertical.
 */
export function spinFor(travelDir, topspin, sidespin = 0) {
  const h = norm(vec(travelDir.x, 0, travelDir.z));
  const topAxis = cross(vec(0, 1, 0), h);   // right-hand axis giving topspin
  return vec(
    topAxis.x * topspin,
    sidespin,
    topAxis.z * topspin,
  );
}
