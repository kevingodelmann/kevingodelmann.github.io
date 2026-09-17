# Flybrain plays Nightfall Survivors

A browser survivors-like game controlled by a real connectome: subgraphs of the
[FlyBrainLab / natverse `malecns`](https://github.com/natverse/malecns) male
fruit fly whole-CNS dataset, driving movement in real time.

## What's actually real here

- **Escape circuit**: LC4/LC6 (looming-detector visual projection neurons) →
  DNp01 (the Giant Fiber escape neuron) + DNp02/03/04/11 → VNC interneurons →
  leg/flight motor neurons. Real synapse weights throughout.
- **Pursuit circuit**: LC10 (small-target/pursuit detectors) → DNa10/DNp10 →
  interneurons → motor neurons, driving movement toward XP gems.
- **Idle/exploratory circuit**: DNg13 → interneurons → motor neurons, driven by
  an internal rhythm instead of vision (spontaneous locomotion during rest).
- **Dopamine-gated learning**: real KC (Kenyon cell) → MBON synapses.
  MBON11 is *depressed* on damage (a simulated PPL101 aversive dopamine pulse) —
  the fly gets more skittish about repeated threats. MBON01 is *potentiated* on
  XP pickup (a simulated PAM reward pulse) — it gets more eager to chase gems
  it's been rewarded for before. Both are the real biological LTD/LTP rule
  (Aso et al.), applied to real synapses, not a metaphor.
- **Brain visualization**: every dot in the dashboard sits at that neuron's
  *real* soma position (from `somaLocation` in the dataset), not an arbitrary
  layout — projected to a front view with a convex-hull brain outline.

## What's NOT real / honest limitations

- The sensory→action mapping (which neuron pool means "turn left") is an
  **engineered choice**, same as comparable public projects like
  [doomfly](https://github.com/nftechie/doomfly). It's not an established
  natural motor readout.
- Dynamics are a simplified linear/instantaneous readout of connectome-weighted
  paths, not a spiking or biophysical simulation.
- XP-seeking direction comes from real game-state (nearest gem), not from an
  actual visual pixel feed like a real fly's eyes would receive.
- This does **not** demonstrate "a fly learning to play a game" in any strong
  sense — it demonstrates a small, real piece of fly wiring driving simple
  reflexes, with two of its real plasticity rules active.

## Running it

This needs a local R backend — it will **not** work by itself on GitHub Pages.

```r
# one-time setup (see the main FlyBrainLab/malecns docs for full instructions)
install.packages("natmanager")
natmanager::install(pkgs="natverse/malecns")
# set your neuprint_token in .Renviron, see https://neuprint.janelia.org
```

```bash
# rebuild the circuits (only needed once, or to change the neuron sets)
Rscript build_circuit_escape.R
Rscript build_circuit_pursuit.R
Rscript build_circuit_idle.R
Rscript build_mb_circuit_aversive.R
Rscript build_mb_circuit_appetitive.R
Rscript build_brain_layout.R
Rscript project_brain_layout.R

# run the bridge server (serves the circuit over HTTP on :8721)
Rscript fly_server.R

# serve this folder statically (any static server works), e.g.:
python -m http.server 8722
```

Then open `dashboard.html` for the side-by-side game + live connectome view,
or `index.html` to just play/watch the game on its own. Click **SURVIVE**,
then toggle **🪰 ON** to hand control to the fly.

## Credits

Built with [Claude Code](https://claude.com/claude-code), using the
[malecns](https://github.com/natverse/malecns) dataset (Janelia FlyEM /
Cambridge Drosophila Connectomics Group) via
[FlyBrainLab](https://github.com/FlyBrainLab/FlyBrainLab) /
[natverse](https://natverse.org).
