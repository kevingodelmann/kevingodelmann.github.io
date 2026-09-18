/**
 * Rendering: scene graph, PBR materials, lighting and post-processing.
 *
 * Upgrades over the previous inline version:
 *  - Physically based materials lit by a real environment map, generated from
 *    a small procedural room so reflections actually match the arena instead
 *    of objects being flat-shaded blobs.
 *  - Bloom on the highlights via EffectComposer.
 *  - Table markings baked into a texture rather than being separate coplanar
 *    meshes hovering 1mm above the surface, which removed the z-fighting.
 *  - A seamed ball texture, so the (now real) spin is actually visible.
 *  - Velocity-scaled motion trail and a paddle that visibly swings.
 */

import * as THREE from 'three';
import { EffectComposer } from 'three/addons/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/addons/postprocessing/RenderPass.js';
import { UnrealBloomPass } from 'three/addons/postprocessing/UnrealBloomPass.js';
import { OutputPass } from 'three/addons/postprocessing/OutputPass.js';
import { RectAreaLightUniformsLib } from 'three/addons/lights/RectAreaLightUniformsLib.js';

// RectAreaLight silently misbehaves unless its BRDF lookup tables are loaded.
RectAreaLightUniformsLib.init();

import { TABLE, BALL_RADIUS, HALF_WIDTH, HALF_LENGTH } from './physics.js';

const BLUE = 0x1b4f80;

/* ------------------------------- textures ---------------------------------- */

function makeTableTexture() {
  // 2.74 x 1.525 -> keep the same aspect so the 20mm lines come out 20mm
  const pxPerM = 360;
  const w = Math.round(TABLE.width * pxPerM);
  const h = Math.round(TABLE.length * pxPerM);
  const c = document.createElement('canvas');
  c.width = w; c.height = h;
  const g = c.getContext('2d');

  const grad = g.createLinearGradient(0, 0, 0, h);
  grad.addColorStop(0, '#1d5588');
  grad.addColorStop(0.5, '#17456f');
  grad.addColorStop(1, '#1d5588');
  g.fillStyle = grad;
  g.fillRect(0, 0, w, h);

  // subtle speckle so the surface isn't a flat wash under close light
  const img = g.getImageData(0, 0, w, h);
  for (let i = 0; i < img.data.length; i += 4) {
    const n = (Math.random() - 0.5) * 8;
    img.data[i] += n; img.data[i + 1] += n; img.data[i + 2] += n;
  }
  g.putImageData(img, 0, 0);

  // ITTF markings: 20mm sidelines and endlines, 3mm centre line
  const line = Math.round(0.02 * pxPerM);
  const centre = Math.max(1, Math.round(0.003 * pxPerM));
  g.fillStyle = '#ffffff';
  g.fillRect(0, 0, line, h);
  g.fillRect(w - line, 0, line, h);
  g.fillRect(0, 0, w, line);
  g.fillRect(0, h - line, w, line);
  g.fillRect((w - centre) / 2, 0, centre, h);

  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  tex.anisotropy = 8;
  return tex;
}

function makeBallTexture() {
  const s = 256;
  const c = document.createElement('canvas');
  c.width = c.height = s;
  const g = c.getContext('2d');
  g.fillStyle = '#fdfaf2';
  g.fillRect(0, 0, s, s);
  // seam: a band around the equator of the UV sphere reads as a rotating line
  g.strokeStyle = 'rgba(150,150,160,0.55)';
  g.lineWidth = 3;
  g.beginPath(); g.moveTo(0, s * 0.5); g.lineTo(s, s * 0.5); g.stroke();
  // a small mark so axial spin is readable too
  g.fillStyle = 'rgba(150,150,160,0.5)';
  g.beginPath(); g.arc(s * 0.25, s * 0.3, s * 0.05, 0, Math.PI * 2); g.fill();
  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

/**
 * Sports-hall floor: a poured rubber surface with a fine speckle. Replaces the
 * purple grid, which read as "tech demo" rather than as a room, and gave the
 * eye no sense of scale.
 */
function makeFloorTexture() {
  const s = 512;
  const c = document.createElement('canvas');
  c.width = c.height = s;
  const g = c.getContext('2d');
  g.fillStyle = '#211a2e';
  g.fillRect(0, 0, s, s);
  // speckle, so grazing light has something to catch and the floor doesn't
  // flatten into a single dead tone
  for (let i = 0; i < 9000; i++) {
    const a = Math.random() * 0.06;
    g.fillStyle = Math.random() < 0.5 ? `rgba(255,245,230,${a})` : `rgba(90,70,140,${a * 1.6})`;
    g.fillRect(Math.random() * s, Math.random() * s, 2, 2);
  }
  const tex = new THREE.CanvasTexture(c);
  tex.wrapS = tex.wrapT = THREE.RepeatWrapping;
  tex.repeat.set(10, 10);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

/** The blue playing-area mat the table stands on, with its white border. */
function makeCourtTexture() {
  const w = 512, h = 1024;
  const c = document.createElement('canvas');
  c.width = w; c.height = h;
  const g = c.getContext('2d');
  g.fillStyle = '#123a5e';
  g.fillRect(0, 0, w, h);
  for (let i = 0; i < 14000; i++) {
    g.fillStyle = `rgba(255,255,255,${Math.random() * 0.03})`;
    g.fillRect(Math.random() * w, Math.random() * h, 2, 2);
  }
  g.strokeStyle = 'rgba(235,240,255,0.55)';
  g.lineWidth = 6;
  g.strokeRect(12, 12, w - 24, h - 24);
  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

/** Perimeter barrier boards, the ones that carry sponsor text in a real hall. */
function makeBoardTexture(label) {
  const w = 1024, h = 128;
  const c = document.createElement('canvas');
  c.width = w; c.height = h;
  const g = c.getContext('2d');
  g.fillStyle = '#0d1b33';
  g.fillRect(0, 0, w, h);
  g.fillStyle = 'rgba(120,160,255,0.10)';
  g.fillRect(0, h - 10, w, 10);
  g.font = 'bold 54px ui-monospace, monospace';
  g.textAlign = 'center';
  g.textBaseline = 'middle';
  for (let i = 0; i < 3; i++) {
    g.fillStyle = i % 2 ? 'rgba(167,120,255,0.85)' : 'rgba(210,225,255,0.75)';
    g.fillText(label, (i + 0.5) * (w / 3), h / 2);
  }
  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

/** The arena's big screen at the far end of the hall. */
function makeScreenTexture() {
  const w = 1024, h = 512;
  const c = document.createElement('canvas');
  c.width = w; c.height = h;
  const g = c.getContext('2d');
  const grad = g.createLinearGradient(0, 0, 0, h);
  grad.addColorStop(0, '#111a3a');
  grad.addColorStop(1, '#0a0f2a');
  g.fillStyle = grad;
  g.fillRect(0, 0, w, h);

  g.strokeStyle = 'rgba(120,150,255,0.18)';
  g.lineWidth = 3;
  g.strokeRect(24, 24, w - 48, h - 48);

  g.textAlign = 'center';
  g.fillStyle = '#8fa4cc';
  g.font = 'bold 82px ui-monospace, monospace';
  g.fillText('FLYPONG', w / 2, 170);
  g.font = 'bold 46px ui-monospace, monospace';
  g.fillStyle = '#7a5bb5';
  g.fillText('WORLD TOUR', w / 2, 240);
  g.font = '34px ui-monospace, monospace';
  g.fillStyle = 'rgba(190,205,240,0.7)';
  g.fillText('connectome-driven', w / 2, 330);

  // a suggestion of a live feed strip along the bottom
  for (let i = 0; i < 16; i++) {
    g.fillStyle = `rgba(120,150,255,${0.05 + (i % 3) * 0.05})`;
    g.fillRect(40 + i * 58, h - 110, 46, 60);
  }
  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

/**
 * A tiny room with glowing ceiling strips, baked to an environment map. Gives
 * every PBR surface something coherent to reflect — the single biggest visual
 * difference from the old flat-lit look.
 */
function makeEnvironment(renderer) {
  const env = new THREE.Scene();
  const box = new THREE.Mesh(
    new THREE.BoxGeometry(14, 7, 14),
    new THREE.MeshBasicMaterial({ color: 0x14102a, side: THREE.BackSide }),
  );
  env.add(box);
  const strip = new THREE.MeshBasicMaterial({ color: 0xfff0d8 });
  for (const z of [-2.2, 0, 2.2]) {
    const m = new THREE.Mesh(new THREE.BoxGeometry(5, 0.05, 0.5), strip);
    m.position.set(0, 3.2, z);
    env.add(m);
  }
  const accent = new THREE.Mesh(
    new THREE.BoxGeometry(0.4, 3, 0.4),
    new THREE.MeshBasicMaterial({ color: 0x6a4bd0 }),
  );
  accent.position.set(-5, 1.5, -5);
  env.add(accent);

  const pmrem = new THREE.PMREMGenerator(renderer);
  const target = pmrem.fromScene(env, 0.04);
  pmrem.dispose();
  return target.texture;
}

/* --------------------------------- build ----------------------------------- */

export function createScene(canvas) {
  const renderer = new THREE.WebGLRenderer({ canvas, antialias: true, powerPreference: 'high-performance' });
  renderer.setPixelRatio(Math.min(devicePixelRatio, 2));
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;
  renderer.toneMapping = THREE.ACESFilmicToneMapping;
  renderer.toneMappingExposure = 0.92;

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x161133);
  // Light fog only. At 0.055 the arena walls 11m out were ~30% fogged into a
  // background that was itself near-black, which is why everything above the
  // table read as an empty void.
  scene.fog = new THREE.FogExp2(0x161133, 0.020);
  scene.environment = makeEnvironment(renderer);

  const camera = new THREE.PerspectiveCamera(50, 1, 0.05, 100);

  /* ---- table ---- */
  const table = new THREE.Group();
  const topMat = new THREE.MeshPhysicalMaterial({
    map: makeTableTexture(),
    roughness: 0.34, metalness: 0.0,
    clearcoat: 0.55, clearcoatRoughness: 0.28,
    envMapIntensity: 0.7,
  });
  const top = new THREE.Mesh(new THREE.BoxGeometry(TABLE.width, 0.03, TABLE.length), topMat);
  top.position.y = TABLE.height - 0.015;
  top.receiveShadow = true;
  table.add(top);

  const apron = new THREE.Mesh(
    new THREE.BoxGeometry(TABLE.width + 0.02, 0.1, TABLE.length + 0.02),
    new THREE.MeshStandardMaterial({ color: 0x0a1b2c, roughness: 0.65, envMapIntensity: 0.4 }),
  );
  apron.position.y = TABLE.height - 0.08;
  apron.castShadow = true; apron.receiveShadow = true;
  table.add(apron);

  const legMat = new THREE.MeshStandardMaterial({ color: 0x15131f, roughness: 0.45, metalness: 0.5 });
  for (const sx of [-1, 1]) for (const sz of [-1, 1]) {
    const leg = new THREE.Mesh(new THREE.BoxGeometry(0.05, TABLE.height - 0.13, 0.05), legMat);
    leg.position.set(sx * (HALF_WIDTH - 0.1), (TABLE.height - 0.13) / 2, sz * (HALF_LENGTH - 0.2));
    leg.castShadow = true;
    table.add(leg);
  }
  scene.add(table);

  /* ---- net ---- */
  const netCanvas = document.createElement('canvas');
  netCanvas.width = 256; netCanvas.height = 48;
  {
    const g = netCanvas.getContext('2d');
    g.clearRect(0, 0, 256, 48);
    g.strokeStyle = 'rgba(232,232,240,0.55)';
    g.lineWidth = 1;
    for (let x = 0; x <= 256; x += 5) { g.beginPath(); g.moveTo(x, 0); g.lineTo(x, 48); g.stroke(); }
    for (let y = 0; y <= 48; y += 5) { g.beginPath(); g.moveTo(0, y); g.lineTo(256, y); g.stroke(); }
    g.fillStyle = '#f2f2f8';
    g.fillRect(0, 0, 256, 4);   // white tape along the top
  }
  const netTex = new THREE.CanvasTexture(netCanvas);
  const netSpan = TABLE.width + TABLE.netOverhang * 2;
  const net = new THREE.Mesh(
    new THREE.PlaneGeometry(netSpan, TABLE.netHeight),
    new THREE.MeshStandardMaterial({ map: netTex, transparent: true, side: THREE.DoubleSide, roughness: 0.9 }),
  );
  net.position.set(0, TABLE.height + TABLE.netHeight / 2, 0);
  scene.add(net);

  const postMat = new THREE.MeshStandardMaterial({ color: 0x1c1c22, metalness: 0.8, roughness: 0.3 });
  for (const s of [-1, 1]) {
    const post = new THREE.Mesh(new THREE.CylinderGeometry(0.009, 0.009, TABLE.netHeight, 12), postMat);
    post.position.set(s * netSpan / 2, TABLE.height + TABLE.netHeight / 2, 0);
    post.castShadow = true;
    scene.add(post);
  }

  /* ---- floor & arena ----
   *
   * Previously this was a black cylinder in near-black fog, so everything above
   * the table was an empty void: the table had no room to sit in, nothing to
   * reflect, and the eye had no scale reference. This builds an actual hall.
   */
  const COURT_W = 6.2, COURT_L = 11.0;   // the marked playing area around the table

  const floor = new THREE.Mesh(
    new THREE.PlaneGeometry(60, 60),
    new THREE.MeshStandardMaterial({
      map: makeFloorTexture(), roughness: 0.72, metalness: 0.05, envMapIntensity: 0.5,
    }),
  );
  floor.rotation.x = -Math.PI / 2;
  floor.receiveShadow = true;
  scene.add(floor);

  const court = new THREE.Mesh(
    new THREE.PlaneGeometry(COURT_W, COURT_L),
    new THREE.MeshStandardMaterial({
      map: makeCourtTexture(), roughness: 0.55, metalness: 0.04, envMapIntensity: 0.7,
    }),
  );
  court.rotation.x = -Math.PI / 2;
  court.position.y = 0.004;            // just clear of the floor, no z-fighting
  court.receiveShadow = true;
  scene.add(court);

  // Perimeter barrier boards. These do most of the work of making the space
  // read as a venue, and they give the table's glossy top something with
  // structure to reflect instead of flat darkness.
  const boardH = 0.60, boardT = 0.05;
  const boardGeo = new THREE.BoxGeometry(1, boardH, boardT);
  const sideBoard = new THREE.MeshStandardMaterial({
    map: makeBoardTexture('FLYPONG'), roughness: 0.5, metalness: 0.1, envMapIntensity: 0.6,
  });
  const endBoard = new THREE.MeshStandardMaterial({
    map: makeBoardTexture('MALECNS'), roughness: 0.5, metalness: 0.1, envMapIntensity: 0.6,
  });
  function addBoard(x, z, len, rotY, mat) {
    const b = new THREE.Mesh(boardGeo, mat);
    b.scale.x = len;
    b.position.set(x, boardH / 2, z);
    b.rotation.y = rotY;
    b.castShadow = true; b.receiveShadow = true;
    scene.add(b);
  }
  addBoard(-COURT_W / 2, 0, COURT_L, Math.PI / 2, sideBoard);
  addBoard( COURT_W / 2, 0, COURT_L, Math.PI / 2, sideBoard);
  addBoard(0, -COURT_L / 2, COURT_W, 0, endBoard);
  addBoard(0,  COURT_L / 2, COURT_W, 0, endBoard);

  // Tiered seating with a crowd, instanced so several hundred spectators cost
  // one draw call. Kept dim and low-contrast: it should register as a full
  // arena in peripheral vision without competing with the ball.
  const tiers = 5;
  const perTier = 46;
  const crowdGeo = new THREE.CapsuleGeometry(0.16, 0.26, 3, 6);
  const crowd = new THREE.InstancedMesh(
    crowdGeo,
    new THREE.MeshStandardMaterial({ roughness: 0.95, metalness: 0 }),
    tiers * perTier * 2,
  );
  const stand = new THREE.Object3D();
  const palette = [0x3b2f57, 0x4a3a63, 0x2e3f5e, 0x53406b, 0x35506b, 0x604a72];
  const col = new THREE.Color();
  let ci = 0;
  for (const dir of [-1, 1]) {
    for (let t = 0; t < tiers; t++) {
      const x = dir * (COURT_W / 2 + 1.3 + t * 0.85);
      const y = 0.45 + t * 0.42;
      for (let i = 0; i < perTier; i++) {
        const z = -COURT_L / 2 - 1 + (i / (perTier - 1)) * (COURT_L + 2);
        stand.position.set(x + (Math.random() - 0.5) * 0.18, y, z + (Math.random() - 0.5) * 0.12);
        stand.rotation.set(0, dir > 0 ? -Math.PI / 2 : Math.PI / 2, 0);
        stand.scale.setScalar(0.9 + Math.random() * 0.25);
        stand.updateMatrix();
        crowd.setMatrixAt(ci, stand.matrix);
        col.setHex(palette[(Math.random() * palette.length) | 0]);
        crowd.setColorAt(ci, col);
        ci++;
      }
    }
  }
  crowd.instanceMatrix.needsUpdate = true;
  scene.add(crowd);

  // The stands themselves, as simple risers under the crowd.
  const riserMat = new THREE.MeshStandardMaterial({
    color: 0x191330, roughness: 0.9, emissive: 0x181240, emissiveIntensity: 0.35,
  });
  for (const dir of [-1, 1]) {
    for (let t = 0; t < tiers; t++) {
      const riser = new THREE.Mesh(new THREE.BoxGeometry(0.85, 0.42 * (t + 1), COURT_L + 2), riserMat);
      riser.position.set(dir * (COURT_W / 2 + 1.3 + t * 0.85), 0.21 * (t + 1), 0);
      riser.receiveShadow = true;
      scene.add(riser);
    }
  }

  // Far walls, to close the box off behind the stands.
  const wallMat = new THREE.MeshStandardMaterial({
    color: 0x1d1738, roughness: 1,
    emissive: 0x241c4e, emissiveIntensity: 0.55,
  });
  const hall = new THREE.Mesh(new THREE.BoxGeometry(26, 11, 34), wallMat);
  hall.material.side = THREE.BackSide;
  hall.position.y = 5.5 - 0.01;
  scene.add(hall);

  // An LED ribbon running along the back of both stands. The upper third of
  // the frame was still solid black because the spotlights only carry 9m and
  // nothing else reached the walls -- emissive geometry is the cheap fix, and
  // it gives the arena a horizon line instead of a void.
  const ribbonMat = new THREE.MeshBasicMaterial({ color: 0x7b5bd6 });
  for (const dir of [-1, 1]) {
    const ribbon = new THREE.Mesh(new THREE.BoxGeometry(0.08, 0.20, COURT_L + 2), ribbonMat);
    ribbon.position.set(dir * (COURT_W / 2 + 1.3 + tiers * 0.85), 2.45, 0);
    scene.add(ribbon);
  }

  // Big screen at the far end: a focal point down the table, and the main
  // source of light on the back wall.
  const screen = new THREE.Mesh(
    new THREE.PlaneGeometry(4.4, 2.2),
    new THREE.MeshBasicMaterial({ map: makeScreenTexture() }),
  );
  screen.position.set(0, 2.05, -13.6);
  scene.add(screen);

  const screenGlow = new THREE.RectAreaLight(0x6f8dff, 4.0, 4.4, 2.2);
  screenGlow.position.set(0, 2.05, -13.5);
  screenGlow.lookAt(0, 1.5, 0);
  scene.add(screenGlow);

  const screenFrame = new THREE.Mesh(
    new THREE.BoxGeometry(4.7, 2.5, 0.14),
    new THREE.MeshStandardMaterial({ color: 0x0d0a18, roughness: 0.8 }),
  );
  screenFrame.position.set(0, 2.05, -13.75);
  scene.add(screenFrame);

  // A dim uplight on the back wall so it reads as a surface, not a black gap.
  const wallWash = new THREE.PointLight(0x5a4d9c, 40, 26, 2);
  wallWash.position.set(0, 4.5, -9);
  scene.add(wallWash);

  /* ---- lights ---- */
  scene.add(new THREE.HemisphereLight(0x6f66a8, 0x140f22, 0.38));

  const key = new THREE.DirectionalLight(0xfff4e2, 1.15);
  key.position.set(1.6, 4.2, 1.4);
  key.castShadow = true;
  key.shadow.mapSize.set(2048, 2048);
  key.shadow.camera.near = 1; key.shadow.camera.far = 9;
  key.shadow.camera.left = -1.6; key.shadow.camera.right = 1.6;
  key.shadow.camera.top = 2.2; key.shadow.camera.bottom = -2.2;
  key.shadow.bias = -0.0009;
  key.shadow.radius = 3;
  scene.add(key);

  const rim = new THREE.DirectionalLight(0x8f7bff, 0.55);
  rim.position.set(-2.5, 2.0, -3);
  scene.add(rim);

  // Overhead rig: visible housings with emissive panels, plus real spotlights
  // aimed at the table. Bloom picks the panels up, which is what sells the
  // "lit arena" look -- the old version had bare emissive strips floating with
  // no fixture around them and cast no light of their own.
  const fixtureMat = new THREE.MeshBasicMaterial({ color: 0xfff3dd });
  const housingMat = new THREE.MeshStandardMaterial({ color: 0x1a1726, roughness: 0.5, metalness: 0.7 });
  for (const z of [-1.15, 0, 1.15]) {
    const rig = new THREE.Group();
    const housing = new THREE.Mesh(new THREE.BoxGeometry(2.6, 0.18, 0.44), housingMat);
    rig.add(housing);
    const panel = new THREE.Mesh(new THREE.BoxGeometry(2.4, 0.03, 0.3), fixtureMat);
    panel.position.y = -0.1;
    rig.add(panel);
    rig.position.set(0, 3.0, z * HALF_LENGTH);
    scene.add(rig);

    const spot = new THREE.SpotLight(0xfff6e8, 7.5, 9, 0.7, 0.6, 1.6);
    spot.position.set(0, 2.95, z * HALF_LENGTH);
    spot.target.position.set(0, TABLE.height, z * HALF_LENGTH);
    scene.add(spot, spot.target);
  }

  // Truss the rig hangs from, so the lights are attached to something.
  const trussMat = new THREE.MeshStandardMaterial({ color: 0x241f38, roughness: 0.6, metalness: 0.6 });
  for (const x of [-1.5, 1.5]) {
    const truss = new THREE.Mesh(new THREE.BoxGeometry(0.09, 0.09, COURT_L), trussMat);
    truss.position.set(x, 3.25, 0);
    scene.add(truss);
  }

  // Ceiling. Without this the upper half of the frame was pure black with a
  // hard horizon line across it -- the hall had walls but no lid, so the eye
  // read it as an open void again.
  const ceiling = new THREE.Mesh(
    new THREE.PlaneGeometry(26, 34),
    new THREE.MeshStandardMaterial({
      color: 0x171230, roughness: 0.95,
      emissive: 0x1d1740, emissiveIntensity: 0.5,
    }),
  );
  ceiling.rotation.x = Math.PI / 2;
  ceiling.position.y = 7.2;
  scene.add(ceiling);

  // Roof girders, so the ceiling has depth rather than being a flat lid.
  for (let i = -4; i <= 4; i++) {
    const girder = new THREE.Mesh(new THREE.BoxGeometry(24, 0.16, 0.16), trussMat);
    girder.position.set(0, 6.95, i * 3.2);
    scene.add(girder);
  }

  // A soft fill from behind the camera. The near paddle faces away from every
  // other source, so it was rendering as a black silhouette in the foreground.
  const fill = new THREE.DirectionalLight(0xbfd0ff, 0.5);
  fill.position.set(0.4, 1.6, 4.5);
  scene.add(fill);

  /* ---- ball ---- */
  const ball = new THREE.Mesh(
    new THREE.SphereGeometry(BALL_RADIUS, 32, 24),
    new THREE.MeshPhysicalMaterial({
      map: makeBallTexture(),
      roughness: 0.42, clearcoat: 0.5, clearcoatRoughness: 0.35,
      envMapIntensity: 0.9,
    }),
  );
  ball.castShadow = true;
  scene.add(ball);

  // motion trail: a ribbon of shrinking spheres whose visibility scales with speed
  const TRAIL = 14;
  const trail = [];
  for (let i = 0; i < TRAIL; i++) {
    const t = new THREE.Mesh(
      new THREE.SphereGeometry(BALL_RADIUS * (1 - i / TRAIL) * 0.85, 10, 8),
      new THREE.MeshBasicMaterial({ color: 0xfff6e0, transparent: true, opacity: 0, depthWrite: false }),
    );
    scene.add(t);
    trail.push(t);
  }

  /* ---- paddles ----
   *
   * The old bat was a flat 40-sided cylinder on a plain cone, which read as a
   * lollipop: a hard-rimmed disc, no neck joining it to the handle, and a
   * handle that was round in section instead of flat. This builds the real
   * thing -- a slightly oval blade with a bevelled edge, a tapered neck, and a
   * flattened flared handle with an end knob.
   */

  /** Rubber sheet: matte with a very fine tooth, so it isn't a plastic disc. */
  function makeRubberTexture(hex) {
    const s = 256;
    const c = document.createElement('canvas');
    c.width = c.height = s;
    const g = c.getContext('2d');
    g.fillStyle = hex;
    g.fillRect(0, 0, s, s);
    for (let i = 0; i < 6000; i++) {
      g.fillStyle = `rgba(0,0,0,${Math.random() * 0.07})`;
      g.fillRect(Math.random() * s, Math.random() * s, 1.5, 1.5);
    }
    const tex = new THREE.CanvasTexture(c);
    tex.colorSpace = THREE.SRGBColorSpace;
    return tex;
  }

  // Blade outline: 157mm x 150mm, very slightly taller than wide, with a neck
  // flowing down into the handle rather than a disc stuck on a stick.
  function bladeShape(rx, ry) {
    const sh = new THREE.Shape();
    const steps = 64;
    for (let i = 0; i <= steps; i++) {
      const a = (i / steps) * Math.PI * 2 - Math.PI / 2;
      // pinch the bottom inward to form the shoulders of the neck
      const pinch = 1 - 0.34 * Math.max(0, -Math.sin(a)) ** 2.2;
      const x = Math.cos(a) * rx * pinch;
      const y = Math.sin(a) * ry;
      if (i === 0) sh.moveTo(x, y); else sh.lineTo(x, y);
    }
    return sh;
  }

  function makePaddle(rubberColor) {
    const g = new THREE.Group();
    const blade = new THREE.Group();

    const woodMat = new THREE.MeshStandardMaterial({
      color: 0xc08e56, roughness: 0.5, envMapIntensity: 0.5,
    });
    const core = new THREE.Mesh(
      new THREE.ExtrudeGeometry(bladeShape(0.0785, 0.075), {
        depth: 0.006, bevelEnabled: true, bevelThickness: 0.0016,
        bevelSize: 0.0016, bevelSegments: 3, curveSegments: 24,
      }),
      woodMat,
    );
    core.position.z = -0.003;
    core.castShadow = true;
    blade.add(core);

    const rubber = (color, z, flip) => {
      const m = new THREE.Mesh(
        new THREE.ExtrudeGeometry(bladeShape(0.0755, 0.0722), {
          depth: 0.0019, bevelEnabled: true, bevelThickness: 0.0006,
          bevelSize: 0.0009, bevelSegments: 2, curveSegments: 24,
        }),
        new THREE.MeshPhysicalMaterial({
          map: makeRubberTexture(color),
          roughness: 0.82, sheen: 0.35, sheenRoughness: 0.7, envMapIntensity: 0.25,
        }),
      );
      m.position.z = z;
      m.scale.z = flip ? -1 : 1;
      m.castShadow = true;
      return m;
    };
    blade.add(rubber(rubberColor, 0.0032, false));
    blade.add(rubber('#131318', -0.0032, true));

    blade.position.y = 0.073;
    g.add(blade);

    // Neck: bridges blade to handle so there is no floating gap at the joint.
    const neck = new THREE.Mesh(new THREE.CylinderGeometry(0.019, 0.0165, 0.032, 14), woodMat);
    neck.scale.x = 0.62;
    neck.position.y = 0.019;
    neck.castShadow = true;
    g.add(neck);

    // Handle: flattened in section (scale.x) and flared toward the end, which
    // is what makes it read as a bat grip rather than a broom handle.
    const gripMat = new THREE.MeshStandardMaterial({ color: 0x53331c, roughness: 0.68 });
    const handle = new THREE.Mesh(new THREE.CylinderGeometry(0.0168, 0.0208, 0.082, 20), gripMat);
    handle.scale.x = 0.60;
    handle.position.y = -0.031;
    handle.castShadow = true;
    g.add(handle);

    const knob = new THREE.Mesh(new THREE.SphereGeometry(0.021, 16, 12), gripMat);
    knob.scale.set(0.60, 0.42, 1);
    knob.position.y = -0.071;
    knob.castShadow = true;
    g.add(knob);

    g.userData.blade = blade;
    return g;
  }

  // Which face points at the camera is decided per frame by animatePaddle,
  // which sets rotation.y from the paddle's facing, so it is not set here.
  const playerPaddle = makePaddle('#c8322b');
  const oppPaddle = makePaddle('#c8322b');
  scene.add(playerPaddle, oppPaddle);

  /* ---- contact shadows (cheap soft AO under the ball and paddles) ---- */
  const blobCanvas = document.createElement('canvas');
  blobCanvas.width = blobCanvas.height = 128;
  {
    const g = blobCanvas.getContext('2d');
    const rad = g.createRadialGradient(64, 64, 0, 64, 64, 64);
    rad.addColorStop(0, 'rgba(0,0,0,0.5)');
    rad.addColorStop(1, 'rgba(0,0,0,0)');
    g.fillStyle = rad; g.fillRect(0, 0, 128, 128);
  }
  const blobTex = new THREE.CanvasTexture(blobCanvas);
  const makeBlob = (size) => {
    const m = new THREE.Mesh(
      new THREE.PlaneGeometry(size, size),
      new THREE.MeshBasicMaterial({ map: blobTex, transparent: true, depthWrite: false }),
    );
    m.rotation.x = -Math.PI / 2;
    scene.add(m);
    return m;
  };
  const ballBlob = makeBlob(0.11);

  /* ---- impact sparks ---- */
  const sparkGeo = new THREE.SphereGeometry(0.005, 6, 5);
  const sparks = [];
  // Hard ceiling on live particles. Their lifetime is advanced by the
  // simulation, but a render stall or a paused tab can still let spawns
  // outpace retirement, and an unbounded pool quietly grew the scene to tens
  // of thousands of meshes during long headless runs.
  const MAX_SPARKS = 240;
  function spark(pos, color) {
    for (let i = 0; i < 12; i++) {
      if (sparks.length >= MAX_SPARKS) {
        const oldest = sparks.shift();
        scene.remove(oldest.m);
        oldest.m.material.dispose();
      }
      const m = new THREE.Mesh(sparkGeo, new THREE.MeshBasicMaterial({ color, transparent: true }));
      m.position.copy(pos);
      scene.add(m);
      sparks.push({
        m,
        v: new THREE.Vector3((Math.random() - 0.5) * 1.8, Math.random() * 1.3, (Math.random() - 0.5) * 1.8),
        life: 0.38,
      });
    }
  }
  function updateSparks(dt) {
    for (let i = sparks.length - 1; i >= 0; i--) {
      const s = sparks[i];
      s.m.position.addScaledVector(s.v, dt);
      s.v.y -= 5 * dt;
      s.life -= dt;
      s.m.material.opacity = Math.max(0, s.life / 0.38);
      if (s.life <= 0) { scene.remove(s.m); s.m.material.dispose(); sparks.splice(i, 1); }
    }
  }

  /* ---- post-processing ---- */
  const composer = new EffectComposer(renderer);
  composer.addPass(new RenderPass(scene, camera));
  const bloom = new UnrealBloomPass(new THREE.Vector2(1, 1), 0.22, 0.8, 0.92);
  composer.addPass(bloom);
  composer.addPass(new OutputPass());

  function resize(w, h) {
    renderer.setSize(w, h, false);
    composer.setSize(w, h);
    bloom.setSize(w, h);
    camera.aspect = w / h;
    camera.updateProjectionMatrix();
  }

  return {
    renderer, scene, camera, composer, resize,
    ball, trail, ballBlob, playerPaddle, oppPaddle,
    spark, updateSparks,
    render: () => composer.render(),
  };
}
