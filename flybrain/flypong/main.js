/**
 * Game loop, paddle control, opponent AI and shot generation.
 *
 * How a shot is produced, and what is "real":
 *   - Flight and bounces are pure physics (drag, Magnus, spin-coupled friction).
 *   - Aiming is assisted: the stroke picks a target on the far half and solves
 *     for the launch that lands there under that same physics. This is a
 *     deliberate game-design choice, not a claim of simulated stroke mechanics
 *     — the controller here is a fly connectome that can only steer laterally,
 *     so it cannot aim a racket face.
 *   - What the paddle actually does still matters: where the ball meets the
 *     face steers placement, lateral paddle speed adds sidespin, and the chosen
 *     stroke sets speed and topspin/backspin.
 */

import * as THREE from 'three';
import * as PH from './physics.js';
import { TABLE, HALF_WIDTH, HALF_LENGTH, BALL_RADIUS, vec } from './physics.js';
import * as R from './rules.js';
import { createScene } from './scene.js';
import { createFlyBrain } from './flybrain.js';

const $ = (id) => document.getElementById(id);

/* ------------------------------ tournament --------------------------------- */

/**
 * Opponent ratings. `speed` is metres per second of lateral movement, and it
 * is the number that decides whether points ever end: the ball takes roughly
 * half a second between paddles, so anything much above ~3 m/s can cover the
 * whole 1.5m table every single time and no placement can ever win a point.
 * Real players can't do that, which is precisely why rallies terminate.
 */
/*
 * Ratings are (lateral speed, how fast anticipation converges, how badly it
 * misreads a shot, how ruthlessly it exploits the open court). An earlier set
 * had round one already converging like a finalist — a perfect-tracking test
 * bot still lost every game to it — so the ladder now actually starts easy.
 */
const TOUR = [
  { flag: '🇯🇵', name: 'Japan',   speed: 1.70, reach: 1.0, err: 0.185, lead: 0.12 },
  { flag: '🇧🇷', name: 'Brazil',  speed: 1.85, reach: 1.3, err: 0.165, lead: 0.20 },
  { flag: '🇫🇷', name: 'France',  speed: 2.00, reach: 1.6, err: 0.145, lead: 0.30 },
  { flag: '🇩🇪', name: 'Germany', speed: 2.15, reach: 2.0, err: 0.120, lead: 0.42 },
  { flag: '🇰🇷', name: 'S.Korea', speed: 2.32, reach: 2.4, err: 0.095, lead: 0.55 },
  { flag: '🇨🇳', name: 'China',   speed: 2.50, reach: 2.9, err: 0.070, lead: 0.68 },
];

/** Lateral paddle speed for the human/connectome side. */
/**
 * How fast the player's paddle can track sideways, in m/s.
 *
 * This is the single most difficulty-sensitive constant in the game. A shot
 * crosses the table in roughly half a second, so covering the full 1.5m width
 * demands ~3 m/s; anything below that makes the widest placements physically
 * unreturnable no matter how well the ball is read. Tuning the opponent
 * ratings barely moves the win rate compared to moving this.
 */
let PLAYER_SPEED = 3.3;

/**
 * How far outside the table a paddle can reach. Table tennis has no sideline
 * in the air, so wide-angle shots legitimately cross well outside the table —
 * if the paddles can't follow them out there, every such shot is an automatic
 * winner and whoever aims wider simply wins. Real players step out; so do these.
 */
const PADDLE_X_LIMIT = HALF_WIDTH + 0.45;

const PLAYER_Z = HALF_LENGTH + 0.20;
const OPP_Z = -(HALF_LENGTH + 0.20);
const PADDLE_RADIUS = 0.082;
const PADDLE_Y = TABLE.height + 0.075;

/* --------------------------------- state ----------------------------------- */

const view = createScene($('cv'));
const brain = createFlyBrain();
const ball = PH.makeBall();

const G = {
  phase: 'title',        // title | serveWait | rally | pointPause | gamePause | matchOver
  timer: 0,
  round: 0,
  wonTour: false,
  match: R.makeMatch(),
  banner: '',
  lastReason: '',
  player: { x: 0, y: PADDLE_Y, z: PLAYER_Z, prevX: 0, vx: 0, stroke: 'drive', aim: { x: 0, y: PADDLE_Y }, reactAt: 0 },
  opp: { x: 0, y: PADDLE_Y, z: OPP_Z, prevX: 0, vx: 0, aim: { x: 0, y: PADDLE_Y }, reactAt: 0 },
};

/* ------------------------------ shot building ------------------------------ */

// Spin in rad/s. Real strokes run roughly 40 rev/s for a drive and 70-90 rev/s
// for a loop, so these are close to life rather than nominal — now that the
// lift model responds to magnitude across the range, they read differently.
const STROKES = {
  drive: { speed: 6.4, topspin: 250, label: 'topspin drive' },
  loop:  { speed: 5.2, topspin: 480, label: 'heavy topspin loop' },
  push:  { speed: 3.8, topspin: -280, label: 'backspin push' },
};

/**
 * Build the outgoing shot from a contact.
 *
 * `faceOffset` is where on the paddle the ball struck, normalised to [-1,1];
 * it steers placement the way hitting off-centre does in reality. `paddleVx`
 * adds sidespin, which then visibly curves the ball in flight.
 */
function buildShot(from, towardZ, stroke, faceOffset, paddleVx, placement = 0, escalation = 0) {
  const s = STROKES[stroke] ?? STROKES.drive;
  const dirSign = Math.sign(towardZ) || -1;

  // Long rallies escalate: each exchange adds a little pace, the way players
  // progressively open up. Without it two well-drilled paddles trade shots
  // down the middle forever and the point never ends.
  const speed = s.speed * (1 + Math.min(0.70, escalation * 0.075));

  // First bounce lands on the far half, deeper for faster strokes. This is a
  // FRACTION of the half-length, so it has to stay below 1 — letting it reach
  // 1.12 aimed every drive roughly 17cm past the end of the table, which is
  // why returns kept landing out.
  const depthFrac = Math.min(0.85, 0.45 + speed * 0.05);
  const targetZ = dirSign * depthFrac * HALF_LENGTH;

  // Placement is what actually wins points: where the ball met the blade,
  // plus deliberate intent to put it away from the opponent.
  const spread = HALF_WIDTH * 0.93;
  const targetX = Math.max(-spread, Math.min(spread, faceOffset * spread * 0.45 + placement * spread));
  const target = vec(targetX, TABLE.height, targetZ);

  const travel = vec(target.x - from.x, 0, target.z - from.z);
  // Sidespin from dragging the bat across the ball. Kept modest: the Magnus
  // force acts over the whole flight, so a large coupling here sends shots
  // curving metres off the side of the table.
  const sidespin = Math.max(-60, Math.min(60, -paddleVx * 13));
  const spin = PH.spinFor(travel, s.topspin, sidespin);

  const solved = PH.solveLaunch(from, target, speed, spin);
  return { vel: solved.vel, spin: solved.spin, label: s.label };
}

/* ------------------------------- prediction -------------------------------- */

/**
 * Simulate a copy of the live ball forward to find where and when it will
 * arrive at a given z-plane. Real prediction against the real model — the old
 * AI just tracked the ball's current x with added noise, which meant it had no
 * idea where a curving or dipping ball was actually going.
 */
function predictArrival(planeZ, maxTime = 2.2) {
  const probe = { pos: { ...ball.pos }, vel: { ...ball.vel }, spin: { ...ball.spin } };
  const dt = 1 / 180;
  let t = 0;
  const approaching = Math.sign(planeZ - probe.pos.z);
  while (t < maxTime) {
    const before = probe.pos.z;
    PH.step(probe, dt);
    t += dt;
    if (probe.pos.y <= BALL_RADIUS + 1e-4) break;
    const crossed = (before - planeZ) * (probe.pos.z - planeZ) <= 0;
    if (crossed && Math.sign(probe.vel.z) === approaching) {
      return { x: probe.pos.x, y: probe.pos.y, t, valid: true };
    }
  }
  return { x: probe.pos.x, y: probe.pos.y, t, valid: false };
}

/* --------------------------------- serving --------------------------------- */

function beginRally() {
  const server = G.match.server;
  // struck from behind the end line after a toss, as the rules require — which
  // also leaves the solver room to bounce on its own half and still clear the net
  const fromZ = server === R.PLAYER ? HALF_LENGTH + 0.10 : -(HALF_LENGTH + 0.10);
  const sign = Math.sign(fromZ);

  ball.pos = vec((Math.random() - 0.5) * 0.4, TABLE.height + 0.30, fromZ);
  ball.vel = vec();
  ball.spin = vec();

  // A legal serve has to bounce on the server's own half, clear the net, and
  // land on the receiver's half. solveServe searches launch speed/elevation
  // under the real model until it finds a trajectory that does all three.
  const spin = PH.spinFor(vec(0, 0, -sign), 80, (Math.random() - 0.5) * 120);
  const solved = PH.solveServe(ball.pos, -sign, spin);
  ball.vel = vec(solved.vel.x + (Math.random() - 0.5) * 0.35, solved.vel.y, solved.vel.z);
  ball.spin = solved.spin;

  R.startRally(G.match);
  G.rallyShots = 0;
  G.player.misread = null;
  G.opp.misread = null;
  G.phase = 'rally';
  updateHud();
}

/* ------------------------------ paddle control ----------------------------- */

/**
 * Control input, normalised.
 *
 * `lateral` is -1..1 across the playable width and `depth` is 0..1 from the
 * near edge of the screen to the far one. Converting pixels to world units is
 * a view concern, so it happens once here at the boundary rather than inside
 * the game logic — which also means the mouse, the connectome and the tests
 * all drive the paddle through exactly the same seam.
 */
const input = { lateral: null, depth: 0.5 };

export function setInput(lateral, depth = input.depth) {
  if (Number.isFinite(lateral)) input.lateral = Math.max(-1, Math.min(1, lateral));
  if (Number.isFinite(depth)) input.depth = Math.max(0, Math.min(1, depth));
}

function pointerTo(clientX, clientY) {
  // A hidden pane or a display:none iframe reports a zero-sized viewport;
  // dividing by that yields Infinity, which used to propagate into the spin
  // vector and permanently NaN the entire simulation.
  const w = innerWidth || 1, h = innerHeight || 1;
  setInput((clientX / w - 0.5) * 2, clientY / h);
}

addEventListener('mousemove', (e) => pointerTo(e.clientX, e.clientY));
addEventListener('touchmove', (e) => {
  pointerTo(e.touches[0].clientX, e.touches[0].clientY);
  e.preventDefault();
}, { passive: false });

const MIN_PADDLE_Y = TABLE.height - 0.02;
const MAX_PADDLE_Y = TABLE.height + 0.55;

/**
 * Refresh a paddle's intended interception point. Prediction is expensive, so
 * it runs on a reaction-time cadence rather than every physics tick — which
 * doubles as the AI's difficulty knob.
 *
 * Tracking HEIGHT as well as lateral position matters: the blade only covers
 * ~16cm vertically, so a paddle pinned at one fixed height simply cannot reach
 * a ball that arrives high or low, and every rally died after one exchange.
 */
function refreshAim(pad, dt, approaching, reaction, errorX, convergence = 1) {
  pad.reactAt -= dt;
  if (pad.reactAt > 0) return;
  pad.reactAt = reaction;

  if (!approaching) {
    // recover toward a neutral ready position between shots
    pad.aim.x *= 0.7;
    pad.aim.y += (PADDLE_Y - pad.aim.y) * 0.5;
    pad.misread = null;
    return;
  }

  // Commit to one misread per incoming shot instead of re-rolling the error
  // every reaction tick — random jitter that is resampled 20x a second just
  // averages out to perfect tracking, which is why the AI never missed.
  // It then converges on the truth as the ball closes, faster for better
  // players, so weak opponents stay wrong-footed and strong ones recover.
  if (pad.misread === null || pad.misread === undefined) {
    pad.misread = (Math.random() - 0.5) * errorX * 2;
    pad.misreadFloor = pad.misread * 0.25;   // anticipation is never perfect, even late
  } else {
    const decayed = pad.misread * (1 - Math.min(0.6, convergence * reaction));
    pad.misread = Math.abs(decayed) > Math.abs(pad.misreadFloor) ? decayed : pad.misreadFloor;
  }

  const hit = predictArrival(pad.z);
  pad.aim.x = (hit.valid ? hit.x : ball.pos.x) + pad.misread;
  pad.aim.y = Math.max(MIN_PADDLE_Y, Math.min(MAX_PADDLE_Y, hit.valid ? hit.y : PADDLE_Y));
}

function moveToward(pad, dt, speed) {
  pad.prevX = pad.x;
  pad.x += Math.max(-1, Math.min(1, (pad.aim.x - pad.x) / 0.2)) * speed * dt;
  pad.x = Math.max(-PADDLE_X_LIMIT, Math.min(PADDLE_X_LIMIT, pad.x));
  pad.y += (pad.aim.y - pad.y) * Math.min(1, dt * 12);
  pad.vx = (pad.x - pad.prevX) / Math.max(dt, 1e-4);
}

function updatePlayer(dt) {
  const p = G.player;
  const approaching = ball.vel.z > 0 && G.phase === 'rally';
  refreshAim(p, dt, approaching, 0.05, 0);

  if (brain.fly.enabled) {
    // lateral steering comes from the connectome; height tracking is an assist
    p.prevX = p.x;
    p.x += (Number.isFinite(brain.fly.x) ? brain.fly.x : 0) * PLAYER_SPEED * dt;
    p.x = Math.max(-PADDLE_X_LIMIT, Math.min(PADDLE_X_LIMIT, p.x));
    p.y += (p.aim.y - p.y) * Math.min(1, dt * 12);
    p.vx = (p.x - p.prevX) / Math.max(dt, 1e-4);
    p.stroke = 'drive';
    return;
  }

  if (input.lateral !== null) {
    p.aim.x = input.lateral * PADDLE_X_LIMIT;
    // screen height picks the stroke: high to attack, low to push
    p.stroke = input.depth < 0.34 ? 'loop' : input.depth > 0.70 ? 'push' : 'drive';
  }
  moveToward(p, dt, PLAYER_SPEED);
}

function updateOpponent(dt) {
  const o = G.opp;
  const cfg = TOUR[Math.min(G.round, TOUR.length - 1)];
  refreshAim(o, dt, ball.vel.z < 0 && G.phase === 'rally', 0.05, cfg.err, cfg.reach);
  moveToward(o, dt, cfg.speed);
}

/* -------------------------------- contacts --------------------------------- */

/**
 * Detect the ball crossing a paddle's plane inside the blade radius. Uses the
 * pre/post positions rather than a proximity test so a fast ball can't pass
 * through the paddle between frames.
 */
function checkPaddle(prevPos, who) {
  const pad = who === R.PLAYER ? G.player : G.opp;
  const planeZ = pad.z;
  const toward = who === R.PLAYER ? 1 : -1;
  if (Math.sign(ball.vel.z) !== toward) return false;

  const crossed = (prevPos.z - planeZ) * (ball.pos.z - planeZ) <= 0;
  if (!crossed) return false;

  const t = Math.abs(prevPos.z - planeZ) / Math.max(1e-6, Math.abs(prevPos.z - ball.pos.z));
  const hx = prevPos.x + (ball.pos.x - prevPos.x) * t;
  const hy = prevPos.y + (ball.pos.y - prevPos.y) * t;

  const dx = hx - pad.x, dy = hy - pad.y;
  if (Math.hypot(dx, dy) > PADDLE_RADIUS) return false;

  const foul = R.registerHit(G.match, who);
  if (foul) { concludePoint(foul); return true; }

  const faceOffset = dx / PADDLE_RADIUS;
  const contact = vec(hx, hy, planeZ);
  const stroke = who === R.PLAYER ? G.player.stroke : 'drive';

  // The AI deliberately plays into the space you have left open, which is what
  // actually creates pressure — previously every shot went down the middle and
  // neither side was ever stretched, so rallies ran forever.
  let placement = 0;
  if (who === R.OPPONENT) {
    const cfg = TOUR[Math.min(G.round, TOUR.length - 1)];
    // How ruthlessly the opponent exploits the open court, scaled by rating —
    // a flat value here made even the first round play like a finalist.
    const openSide = -Math.sign(G.player.x || (Math.random() - 0.5));
    placement = openSide * (0.12 + cfg.lead * 0.78) + (Math.random() - 0.5) * 0.22;
  } else {
    // The player's side aims into the opponent's open court too. Without this
    // the AI placed every ball and the player placed none, so long rallies
    // were a one-sided grind no matter how well the player moved. Deliberate
    // off-centre contact (faceOffset, above) still steers the shot on top.
    const openSide = -Math.sign(G.opp.x || (Math.random() - 0.5));
    placement = openSide * 0.5 + (Math.random() - 0.5) * 0.25;
  }

  G.rallyShots = (G.rallyShots || 0) + 1;
  const shot = buildShot(contact, -toward, stroke, faceOffset, pad.vx, placement, G.rallyShots);

  ball.pos = vec(hx, hy, planeZ + toward * 0.012);
  ball.vel = shot.vel;
  ball.spin = shot.spin;

  view.spark(new THREE.Vector3(hx, hy, planeZ), who === R.PLAYER ? 0xffcf5c : 0x59d2ff);
  swing(who === R.PLAYER ? view.playerPaddle : view.oppPaddle);
  return true;
}

/* -------------------------------- scoring ---------------------------------- */

function concludePoint(result) {
  if (!result) return;
  G.lastReason = result.reason;

  if (result.point === R.PLAYER) brain.reward();

  if (result.matchWon) {
    if (result.point === R.PLAYER) {
      G.round += 1;
      if (G.round >= TOUR.length) { G.wonTour = true; G.round = TOUR.length - 1; }
    }
    G.phase = 'matchOver';
    G.timer = 2.6;
    showTitle(result.point === R.PLAYER
      ? (G.wonTour ? 'World Tour champion — every country beaten.'
                   : `Match won ${G.match.games.player}-${G.match.games.opponent}. Next: ${TOUR[G.round].flag} ${TOUR[G.round].name}.`)
      : `Lost the match ${G.match.games.player}-${G.match.games.opponent}. Try again?`);
    return;
  }

  if (result.gameWon) {
    G.phase = 'gamePause';
    G.timer = 2.0;
    G.banner = `GAME TO ${result.point === R.PLAYER ? 'YOU' : 'OPPONENT'} — ${G.match.games.player}-${G.match.games.opponent}`;
  } else {
    G.phase = 'pointPause';
    G.timer = 0.85;
    const mp = R.isMatchPoint(G.match), gp = R.isGamePoint(G.match);
    G.banner = mp ? `MATCH POINT — ${mp === R.PLAYER ? 'YOU' : 'OPPONENT'}`
             : gp ? `GAME POINT — ${gp === R.PLAYER ? 'YOU' : 'OPPONENT'}`
             : '';
  }
  updateHud();
}

/* ---------------------------------- HUD ------------------------------------ */

function updateHud() {
  const m = G.match;
  $('score').textContent = `${m.points.player} : ${m.points.opponent}`;
  const cfg = TOUR[Math.min(G.round, TOUR.length - 1)];
  $('round').textContent =
    `Round ${G.round + 1}/${TOUR.length} vs ${cfg.flag} ${cfg.name} · `
    + `Games ${m.games.player}-${m.games.opponent} (best of 5) · `
    + `${m.server === R.PLAYER ? 'your serve' : 'their serve'}`;
  $('banner').textContent = G.banner;
  $('banner').style.opacity = G.banner ? 1 : 0;

  const spinMag = PH.len(ball.spin);
  const topspin = ball.spin.x * Math.sign(ball.vel.z || 1);
  $('telemetry').textContent =
    `${(PH.len(ball.vel)).toFixed(1)} m/s · spin ${(spinMag / (2 * Math.PI)).toFixed(0)} rev/s `
    + `${spinMag < 20 ? '' : topspin < 0 ? '(topspin)' : '(backspin)'} · ${STROKES[G.player.stroke].label}`;
}

function showTitle(message) {
  $('title').style.display = 'flex';
  $('title-msg').textContent = message;
  $('trophy').style.display = G.wonTour ? 'block' : 'none';
  renderChips();
}

function renderChips() {
  $('opponents').innerHTML = TOUR.map((o, i) => {
    const cls = i < G.round ? 'done' : i === G.round ? 'current' : '';
    return `<div class="chip ${cls}">${o.flag}<span>${o.name}</span></div>`;
  }).join('');
}

/* ------------------------------ paddle visuals ----------------------------- */

const swingState = new WeakMap();
function swing(group) { swingState.set(group, 0.001); }

function animatePaddle(group, pad, facing, dt) {
  const s = swingState.get(group) ?? 0;
  if (s > 0) {
    const next = s + dt / 0.22;
    swingState.set(group, next >= 1 ? 0 : next);
  }
  const phase = s > 0 ? Math.sin(Math.min(1, s) * Math.PI) : 0;

  group.position.set(pad.x, pad.y - 0.075, pad.z);
  // blade closes over the ball through the stroke, and the whole paddle
  // follows through forward — so a hit reads as a swing, not a teleport
  group.rotation.y = facing > 0 ? Math.PI : 0;
  group.rotation.x = -0.25 - phase * 0.55;
  group.position.z -= facing * phase * 0.10;
  group.position.y += phase * 0.045;
}

/* --------------------------------- lifecycle -------------------------------- */

function startMatch() {
  if (G.wonTour) { G.wonTour = false; G.round = 0; }
  G.match = R.makeMatch();
  G.match.startingServer = R.PLAYER;
  G.match.server = R.PLAYER;
  G.player.x = 0; G.opp.x = 0; G.opp.target = 0;
  G.banner = '';
  $('title').style.display = 'none';
  G.phase = 'serveWait';
  G.timer = 0.7;
  updateHud();
}

$('serve').addEventListener('click', startMatch);
$('flybtn').addEventListener('click', () => {
  brain.fly.enabled = !brain.fly.enabled;
  brain.fly.failures = 0;
  syncFlyButton();
});
function syncFlyButton() {
  $('flybtn').textContent = brain.fly.enabled ? '🪰 ON' : '🪰 OFF';
  $('flybtn').classList.toggle('active', brain.fly.enabled);
  $('flyhud').style.display = brain.fly.enabled ? 'block' : 'none';
}

setInterval(() => {
  if (G.phase === 'rally') brain.poll(ball.pos.x, G.player.x);
  if (brain.fly.enabled) {
    $('flyhud').textContent =
      `pursuit circuit steering · appetitive drive ${(brain.fly.potentiation * 100).toFixed(0)}%`;
  }
  syncFlyButton();
}, 40);

/* ---------------------------------- loop ------------------------------------ */

const FIXED = 1 / 120;
let acc = 0, last = performance.now();

function frame(now) {
  const wall = Math.min(0.1, (now - last) / 1000);
  last = now;
  acc += wall;

  let guard = 0;
  while (acc >= FIXED && guard++ < 8) {
    acc -= FIXED;
    tick(FIXED);
  }
  draw(wall);
  requestAnimationFrame(frame);
}

function tick(dt) {
  // Effects advance on the simulation clock, so they expire correctly whether
  // or not a frame is being drawn.
  view.updateSparks(dt);
  if (G.phase === 'title') return;

  // Advance on expiry rather than only from inside a `timer > 0` branch: a
  // timer that was already at zero used to leave the state machine wedged in
  // its waiting phase forever with nothing to nudge it.
  if (G.phase !== 'rally') {
    if (G.timer > 0) G.timer -= dt;
    if (G.timer <= 0) {
      G.timer = 0;
      if (G.phase === 'serveWait') { beginRally(); }
      else if (G.phase === 'pointPause') { G.banner = ''; G.phase = 'serveWait'; G.timer = 0.6; }
      else if (G.phase === 'gamePause') {
        R.startNextGame(G.match); G.banner = ''; G.phase = 'serveWait'; G.timer = 0.9; updateHud();
      } else if (G.phase === 'matchOver') { G.phase = 'title'; }
    }
  }

  updatePlayer(dt);
  updateOpponent(dt);

  if (G.phase !== 'rally') return;

  // Safety net: a non-finite ball state can never recover on its own — every
  // collision test silently fails and the rally hangs forever with no way out.
  // Cheaper to notice it and replay the point than to leave the game wedged.
  if (![ball.pos.x, ball.pos.y, ball.pos.z, ball.vel.x, ball.vel.y, ball.vel.z]
        .every(Number.isFinite)) {
    console.warn('flypong: non-finite ball state, replaying the point');
    G.phase = 'serveWait';
    G.timer = 0.4;
    return;
  }

  const prev = { ...ball.pos };
  const events = PH.step(ball, dt);

  // There is deliberately no sideline fault here. Table tennis has no side
  // boundary in the air — a wide-angle shot that bounced legally in the corner
  // and then crosses outside the sideline is a good shot, and may even go
  // around the net post. Whether it counts is decided solely by where it
  // bounces, which the rules layer already handles; adding an arbitrary width
  // cut-off was ending ~40% of points on shots that were perfectly legal.
  if (checkPaddle(prev, R.PLAYER)) return;
  if (checkPaddle(prev, R.OPPONENT)) return;

  for (const e of events) {
    if (e.type === 'net') view.spark(new THREE.Vector3(e.x, e.y, 0), 0xf0f0f6);
    if (e.type === 'bounce') view.spark(new THREE.Vector3(e.x, TABLE.height, e.z), 0x9fd8ff);
  }

  const result = R.applyEvents(G.match, events);
  if (result) concludePoint(result);
}

const spinQuat = new THREE.Quaternion();
const spinAxis = new THREE.Vector3();

function draw(dt) {
  view.ball.position.set(ball.pos.x, ball.pos.y, ball.pos.z);

  // rotate the mesh by the real angular velocity, so the spin you see is the
  // spin that is curving the trajectory
  const w = PH.len(ball.spin);
  if (w > 1e-3) {
    spinAxis.set(ball.spin.x, ball.spin.y, ball.spin.z).normalize();
    spinQuat.setFromAxisAngle(spinAxis, w * dt);
    view.ball.quaternion.premultiply(spinQuat);
  }

  // trail length tracks speed, so a fast drive streaks and a slow push doesn't
  const speed = PH.len(ball.vel);
  const strength = Math.min(1, speed / 7);
  for (let i = view.trail.length - 1; i > 0; i--) {
    view.trail[i].position.copy(view.trail[i - 1].position);
  }
  view.trail[0].position.copy(view.ball.position);
  view.trail.forEach((t, i) => {
    t.material.opacity = G.phase === 'rally'
      ? strength * 0.3 * (1 - i / view.trail.length)
      : 0;
  });

  view.ballBlob.position.set(ball.pos.x, TABLE.height + 0.002, ball.pos.z);
  const drop = Math.max(0, ball.pos.y - TABLE.height);
  view.ballBlob.material.opacity = Math.abs(ball.pos.z) < HALF_LENGTH && Math.abs(ball.pos.x) < HALF_WIDTH
    ? Math.max(0, 0.5 - drop * 0.9) : 0;

  animatePaddle(view.playerPaddle, G.player, 1, dt);
  animatePaddle(view.oppPaddle, G.opp, -1, dt);

  // camera sits behind the player and eases with the rally
  // Far enough back that your own bat doesn't fill a quarter of the screen,
  // and high enough to see the far half of the table clearly.
  const cam = view.camera;
  const targetX = G.player.x * 0.3;
  cam.position.x += (targetX - cam.position.x) * Math.min(1, dt * 4);
  cam.position.y = TABLE.height + 0.92;
  cam.position.z = PLAYER_Z + 1.30;
  cam.lookAt(G.player.x * 0.1, TABLE.height + 0.02, -0.35);

  if (G.phase === 'rally') updateHud();
  view.render();
}

function onResize() { view.resize(innerWidth, innerHeight); }
addEventListener('resize', onResize);
onResize();

renderChips();
syncFlyButton();
$('loading').style.display = 'none';
$('title').style.display = 'flex';
requestAnimationFrame(frame);

// expose for the headless physics/rule tests and the dashboard harness
window.FLYPONG = {
  G, ball, PH, R, TOUR, brain, view,
  startMatch, beginRally, predictArrival, buildShot, tick, FIXED, setInput,
  setPlayerSpeed: (v) => { PLAYER_SPEED = v; },
  getPlayerSpeed: () => PLAYER_SPEED,
};
