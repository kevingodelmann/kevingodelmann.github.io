/**
 * ITTF match rules and scoring, as a state machine over physics events.
 *
 * Kept separate from physics and rendering so the rule set can be read in one
 * place. Everything in here is a real rule:
 *
 *  - A game is to 11, win by 2.
 *  - A match is best-of-5 games (first to 3). Not a single game to 11.
 *  - Service alternates every 2 points; every point at deuce (10-10 or beyond).
 *  - A serve must bounce on the server's own half, then the receiver's half.
 *  - You may not volley: the ball must bounce on your half before you return it.
 *  - Letting it bounce twice on your half loses the point.
 *  - Into the net, off the end, or wide is a fault.
 */

export const POINTS_TO_WIN_GAME = 11;
export const GAMES_TO_WIN_MATCH = 3;   // best of 5

export const PLAYER = 'player';
export const OPPONENT = 'opponent';
export const other = (who) => (who === PLAYER ? OPPONENT : PLAYER);

/** The player defends +z, the opponent defends -z. */
export const sideOfZ = (z) => (z > 0 ? PLAYER : OPPONENT);

export function makeMatch() {
  return {
    points: { player: 0, opponent: 0 },
    games: { player: 0, opponent: 0 },
    server: PLAYER,
    startingServer: PLAYER,
    rally: null,
    lastPointWinner: null,
    over: false,
    winner: null,
  };
}

/**
 * A rally tracks who last touched the ball and how many times it has bounced
 * on the current half, which is all the no-volley and double-bounce rules need.
 */
export function startRally(match) {
  match.rally = {
    phase: 'serve',          // 'serve' until it has bounced on both halves
    lastHitBy: match.server,
    bouncesOnCurrentSide: 0,
    servedFrom: match.server,
    serveBouncedOwnSide: false,
    dead: false,
  };
  return match.rally;
}

function totalPoints(match) {
  return match.points.player + match.points.opponent;
}

/** Service alternates every 2 points, or every point once both are at 10+. */
export function updateServer(match) {
  const p = match.points.player, o = match.points.opponent;
  const deuce = p >= POINTS_TO_WIN_GAME - 1 && o >= POINTS_TO_WIN_GAME - 1;
  const interval = deuce ? 1 : 2;
  const handovers = Math.floor(totalPoints(match) / interval);
  match.server = handovers % 2 === 0 ? match.startingServer : other(match.startingServer);
}

export function awardPoint(match, who, reason) {
  if (match.rally) match.rally.dead = true;
  match.points[who] += 1;
  match.lastPointWinner = who;

  const mine = match.points[who], theirs = match.points[other(who)];
  const gameWon = mine >= POINTS_TO_WIN_GAME && mine - theirs >= 2;

  const result = { point: who, reason, gameWon: false, matchWon: false };
  if (gameWon) {
    match.games[who] += 1;
    result.gameWon = true;
    if (match.games[who] >= GAMES_TO_WIN_MATCH) {
      match.over = true;
      match.winner = who;
      result.matchWon = true;
    }
  } else {
    updateServer(match);
  }
  return result;
}

/** Between games the loser of the previous game serves first. */
export function startNextGame(match) {
  match.points.player = 0;
  match.points.opponent = 0;
  match.startingServer = other(match.startingServer);
  match.server = match.startingServer;
  match.rally = null;
}

export function isGamePoint(match) {
  const { player, opponent } = match.points;
  const atGamePoint = (a, b) => a >= POINTS_TO_WIN_GAME - 1 && a - b >= 1;
  if (atGamePoint(player, opponent)) return PLAYER;
  if (atGamePoint(opponent, player)) return OPPONENT;
  return null;
}

export function isMatchPoint(match) {
  const gp = isGamePoint(match);
  if (!gp) return null;
  return match.games[gp] === GAMES_TO_WIN_MATCH - 1 ? gp : null;
}

/**
 * Feed physics events through the rules. Returns a point result, or null if
 * the rally is still live.
 */
export function applyEvents(match, events) {
  const rally = match.rally;
  if (!rally || rally.dead) return null;

  for (const e of events) {
    if (e.type === 'bounce') {
      const side = e.side > 0 ? PLAYER : OPPONENT;

      if (rally.phase === 'serve') {
        // A serve has to touch the server's half first, then cross.
        if (!rally.serveBouncedOwnSide) {
          if (side === rally.servedFrom) {
            rally.serveBouncedOwnSide = true;
            continue;
          }
          // never touched the server's own half
          return awardPoint(match, other(rally.servedFrom), 'serve did not bounce on the server\'s half');
        }
        rally.phase = 'rally';
        rally.bouncesOnCurrentSide = 1;
        continue;
      }

      if (side === rally.lastHitBy) {
        // came back down on the hitter's own half without crossing
        return awardPoint(match, other(rally.lastHitBy), 'return did not cross the net');
      }

      rally.bouncesOnCurrentSide += 1;
      if (rally.bouncesOnCurrentSide >= 2) {
        return awardPoint(match, rally.lastHitBy, 'double bounce — not returned in time');
      }
    }

    if (e.type === 'net') {
      return awardPoint(match, other(rally.lastHitBy), 'into the net');
    }

    if (e.type === 'floor') {
      const missedBy = other(rally.lastHitBy);
      // If it had already bounced on the receiver's half, they simply missed it.
      // If not, the hitter put it off the table.
      const reason = rally.bouncesOnCurrentSide >= 1 ? 'missed the return' : 'off the table';
      const winner = rally.bouncesOnCurrentSide >= 1 ? rally.lastHitBy : missedBy;
      return awardPoint(match, winner, reason);
    }
  }
  return null;
}

/**
 * Called when a paddle makes contact. Enforces the no-volley rule and resets
 * the bounce counter for the other half.
 */
export function registerHit(match, who) {
  const rally = match.rally;
  if (!rally || rally.dead) return null;

  const servingStroke = rally.phase === 'serve' && !rally.serveBouncedOwnSide && who === rally.servedFrom;
  if (!servingStroke && rally.bouncesOnCurrentSide < 1) {
    return awardPoint(match, other(who), 'volley — the ball must bounce on your half first');
  }

  rally.lastHitBy = who;
  rally.bouncesOnCurrentSide = 0;
  if (rally.phase === 'serve' && rally.serveBouncedOwnSide) rally.phase = 'rally';
  return null;
}
