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

function makeFloorTexture() {
  const s = 512;
  const c = document.createElement('canvas');
  c.width = c.height = s;
  const g = c.getContext('2d');
  g.fillStyle = '#0d0a18';
  g.fillRect(0, 0, s, s);
  g.strokeStyle = 'rgba(150,120,220,0.10)';
  g.lineWidth = 2;
  for (let i = 0; i <= s; i += 64) {
    g.beginPath(); g.moveTo(i, 0); g.lineTo(i, s); g.stroke();
    g.beginPath(); g.moveTo(0, i); g.lineTo(s, i); g.stroke();
  }
  const tex = new THREE.CanvasTexture(c);
  tex.wrapS = tex.wrapT = THREE.RepeatWrapping;
  tex.repeat.set(8, 8);
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
  renderer.toneMappingExposure = 1.25;

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x07050f);
  scene.fog = new THREE.FogExp2(0x07050f, 0.055);
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

  /* ---- floor & arena ---- */
  const floor = new THREE.Mesh(
    new THREE.PlaneGeometry(40, 40),
    new THREE.MeshStandardMaterial({ map: makeFloorTexture(), color: 0x8a7ab8, roughness: 0.88, metalness: 0.05, envMapIntensity: 0.35 }),
  );
  floor.rotation.x = -Math.PI / 2;
  floor.receiveShadow = true;
  scene.add(floor);

  const arena = new THREE.Mesh(
    new THREE.CylinderGeometry(11, 11, 8, 40, 1, true),
    new THREE.MeshStandardMaterial({ color: 0x0f0b1e, roughness: 1, side: THREE.BackSide }),
  );
  arena.position.y = 3;
  scene.add(arena);

  /* ---- lights ---- */
  scene.add(new THREE.HemisphereLight(0x8a7fc0, 0x0a0814, 0.55));

  const key = new THREE.DirectionalLight(0xfff4e2, 2.1);
  key.position.set(1.6, 4.2, 1.4);
  key.castShadow = true;
  key.shadow.mapSize.set(2048, 2048);
  key.shadow.camera.near = 1; key.shadow.camera.far = 9;
  key.shadow.camera.left = -1.6; key.shadow.camera.right = 1.6;
  key.shadow.camera.top = 2.2; key.shadow.camera.bottom = -2.2;
  key.shadow.bias = -0.0009;
  key.shadow.radius = 3;
  scene.add(key);

  const rim = new THREE.DirectionalLight(0x8f7bff, 0.8);
  rim.position.set(-2.5, 2.0, -3);
  scene.add(rim);

  const fixtureMat = new THREE.MeshBasicMaterial({ color: 0xfff3dd });
  for (const z of [-1.0, 0, 1.0]) {
    const fx = new THREE.Mesh(new THREE.BoxGeometry(1.5, 0.04, 0.16), fixtureMat);
    fx.position.set(0, 2.9, z * HALF_LENGTH);
    scene.add(fx);
  }

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

  /* ---- paddles ---- */
  function makePaddle(rubberColor) {
    const g = new THREE.Group();
    const blade = new THREE.Group();
    const wood = new THREE.MeshStandardMaterial({ color: 0x8a5f33, roughness: 0.55, envMapIntensity: 0.5 });
    const edge = new THREE.Mesh(new THREE.CylinderGeometry(0.0825, 0.0825, 0.007, 40), wood);
    edge.rotation.x = Math.PI / 2;
    edge.castShadow = true;
    blade.add(edge);

    const rubber = (color, z) => {
      const m = new THREE.Mesh(
        new THREE.CylinderGeometry(0.079, 0.079, 0.0035, 40),
        new THREE.MeshPhysicalMaterial({ color, roughness: 0.75, sheen: 0.4, sheenRoughness: 0.6, envMapIntensity: 0.35 }),
      );
      m.rotation.x = Math.PI / 2;
      m.position.z = z;
      m.castShadow = true;
      return m;
    };
    blade.add(rubber(rubberColor, 0.0052));
    blade.add(rubber(0x101014, -0.0052));
    blade.position.y = 0.075;
    g.add(blade);

    const handle = new THREE.Mesh(
      new THREE.CylinderGeometry(0.0155, 0.021, 0.1, 16),
      new THREE.MeshStandardMaterial({ color: 0x6b4526, roughness: 0.7 }),
    );
    handle.position.y = 0.005;
    handle.castShadow = true;
    g.add(handle);

    g.userData.blade = blade;
    return g;
  }

  const playerPaddle = makePaddle(0xd2332c);
  const oppPaddle = makePaddle(0xd2332c);
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
  const bloom = new UnrealBloomPass(new THREE.Vector2(1, 1), 0.34, 0.75, 0.85);
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
