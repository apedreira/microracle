/*
 * MICRORACLE - loading animation: bacilli dividing by binary fission
 * (2 -> 512 cells, 8 generations) with collisions and auto-zoom.
 * Drawn on every <canvas class="bacilli-loader"> placed by loading_spinner()
 * (modules/common/helpers.R), with the "Working..." label (blinking dots)
 * drawn below the colony in the same canvas.
 * Only visible canvases are simulated, and each spinner restarts from 2 cells
 * every time it appears.
 */
(function () {
  // Logical size of the drawing (the canvas is scaled by CSS): the colony
  // lives in the top COLONY_H pixels and the label below it
  var W = 680, H = 520, COLONY_H = 430, LABEL_Y = 475;

  var PALETTE = ['#48A868', '#DE881D', '#31B896', '#4589C4', '#262626'];
  var STROKE_COLOR = '#1a1a1a';

  // Geometry of each bacillus (rounded rod)
  var BASE_RADIUS = 12;   // radius of the capsule
  var BASE_CYL_LEN = 22;  // initial length of the central cylinder
  var MAX_CYL_LEN = 50;   // length right before dividing
  var MAX_GENERATIONS = 8;
  var SPEED = 2.0;

  var states = new WeakMap();

  function createBacillus(x, y, angle, gen, colorIndex, progress) {
    return {
      x: x, y: y, vx: 0, vy: 0, angle: angle, gen: gen,
      colorIndex: colorIndex % PALETTE.length,
      progress: progress || 0,
      rateFactor: 0.94 + Math.random() * 0.12
    };
  }

  function newState() {
    return {
      cells: [
        createBacillus(-25, 0, -0.05, 0, 0, 0.04),
        createBacillus(25, 0, 0.05, 0, 1, 0)
      ],
      resetHoldTimer: 0,
      cameraScale: 1,
      lastTime: performance.now()
    };
  }

  // Closest points between two 2D segments (capsule-capsule collisions)
  function closestPointsSegments(ax, ay, bx, by, cx, cy, dx, dy) {
    var ux = bx - ax, uy = by - ay, vx = dx - cx, vy = dy - cy, wx = ax - cx, wy = ay - cy;
    var a = ux * ux + uy * uy, b = ux * vx + uy * vy, c = vx * vx + vy * vy;
    var d = ux * wx + uy * wy, e = vx * wx + vy * wy;
    var D = a * c - b * b;
    var sN, sD = D, tN, tD = D;

    if (D < 1e-6) {
      sN = 0; sD = 1; tN = e; tD = c;
    } else {
      sN = b * e - c * d;
      tN = a * e - b * d;
      if (sN < 0) { sN = 0; tN = e; tD = c; }
      else if (sN > sD) { sN = sD; tN = e + b; tD = c; }
    }
    if (tN < 0) {
      tN = 0;
      if (-d < 0) sN = 0;
      else if (-d > a) sN = sD;
      else { sN = -d; sD = a; }
    } else if (tN > tD) {
      tN = tD;
      if (-d + b < 0) sN = 0;
      else if (-d + b > a) sN = sD;
      else { sN = -d + b; sD = a; }
    }
    var sc = Math.abs(sN) < 1e-6 ? 0 : sN / sD;
    var tc = Math.abs(tN) < 1e-6 ? 0 : tN / tD;
    return { p1x: ax + sc * ux, p1y: ay + sc * uy, p2x: cx + tc * vx, p2y: cy + tc * vy, sc: sc, tc: tc };
  }

  function update(s, dt) {
    var cells = s.cells;
    var growthRate = 0.32 * SPEED;
    var newDaughters = [];
    var allFinished = true;
    var i, j, c;

    // Growth and binary fission
    for (i = 0; i < cells.length; i++) {
      c = cells[i];
      if (c.gen >= MAX_GENERATIONS) continue;
      allFinished = false;
      c.progress += dt * growthRate * c.rateFactor;
      if (c.progress < 1) continue;

      var cosA = Math.cos(c.angle), sinA = Math.sin(c.angle);
      var splitOffset = MAX_CYL_LEN * 0.27 + BASE_RADIUS * 0.46;
      var nextGen = c.gen + 1;
      var color2 = (c.colorIndex + 1 + (nextGen % 2)) % PALETTE.length;
      var ox = c.x, oy = c.y, oa = c.angle;

      c.x = ox - cosA * splitOffset;
      c.y = oy - sinA * splitOffset;
      c.angle = oa + (Math.random() - 0.5) * 0.26;
      c.gen = nextGen;
      c.progress = 0;
      c.vx -= cosA * 18 * SPEED;
      c.vy -= sinA * 18 * SPEED;

      var daughter = createBacillus(ox + cosA * splitOffset, oy + sinA * splitOffset,
                                    oa + (Math.random() - 0.5) * 0.26, nextGen, color2, 0);
      daughter.vx = cosA * 18 * SPEED;
      daughter.vy = sinA * 18 * SPEED;
      newDaughters.push(daughter);
    }
    for (i = 0; i < newDaughters.length; i++) cells.push(newDaughters[i]);

    // 512 bacilli reached: short pause and restart the loop
    if (allFinished) {
      s.resetHoldTimer += dt * SPEED;
      if (s.resetHoldTimer > 2.2) return newState();
    }

    // Physical push between bacilli (2D capsules)
    var n = cells.length;
    var halfL = new Float32Array(n), cosArr = new Float32Array(n), sinArr = new Float32Array(n);
    for (i = 0; i < n; i++) {
      c = cells[i];
      halfL[i] = (BASE_CYL_LEN + (MAX_CYL_LEN - BASE_CYL_LEN) * c.progress) * 0.5;
      cosArr[i] = Math.cos(c.angle);
      sinArr[i] = Math.sin(c.angle);
    }

    var minDist = BASE_RADIUS * 2.08, minDistSq = minDist * minDist;
    var maxBound = MAX_CYL_LEN + BASE_RADIUS * 2.4, maxBoundSq = maxBound * maxBound;
    var passes = n > 256 ? 1 : 2;

    for (var pass = 0; pass < passes; pass++) {
      for (i = 0; i < n; i++) {
        var c1 = cells[i], h1 = halfL[i];
        var ax = c1.x - cosArr[i] * h1, ay = c1.y - sinArr[i] * h1;
        var bx = c1.x + cosArr[i] * h1, by = c1.y + sinArr[i] * h1;

        for (j = i + 1; j < n; j++) {
          var c2 = cells[j];
          var dxC = c2.x - c1.x, dyC = c2.y - c1.y;
          if (dxC * dxC + dyC * dyC > maxBoundSq) continue;

          var h2 = halfL[j];
          var cp = closestPointsSegments(ax, ay, bx, by,
            c2.x - cosArr[j] * h2, c2.y - sinArr[j] * h2,
            c2.x + cosArr[j] * h2, c2.y + sinArr[j] * h2);
          var rx = cp.p2x - cp.p1x, ry = cp.p2y - cp.p1y;
          var dSq = rx * rx + ry * ry;
          if (dSq >= minDistSq) continue;

          var d = Math.sqrt(dSq);
          if (d < 0.001) {
            rx = Math.random() - 0.5;
            ry = Math.random() - 0.5;
            d = Math.hypot(rx, ry) || 1;
          }
          var push = (minDist - d) * 0.44, nx = rx / d, ny = ry / d;
          c1.x -= nx * push; c1.y -= ny * push;
          c2.x += nx * push; c2.y += ny * push;

          // Natural rotation when bacilli slide against each other
          c1.angle += (cp.sc - 0.5) * (cosArr[i] * (-ny) - sinArr[i] * (-nx)) * 0.045;
          c2.angle += (cp.tc - 0.5) * (cosArr[j] * ny - sinArr[j] * nx) * 0.045;
        }
      }
    }

    // Move, damp, keep the colony centred and measure it for the auto-zoom
    var maxColonyRadius = 55;
    for (i = 0; i < n; i++) {
      c = cells[i];
      c.x += c.vx * dt; c.y += c.vy * dt;
      c.vx *= 0.85; c.vy *= 0.85;
      c.x *= 0.999; c.y *= 0.999;
      var r = Math.hypot(c.x, c.y) + halfL[i] + BASE_RADIUS;
      if (r > maxColonyRadius) maxColonyRadius = r;
    }

    var fitRadius = Math.min(W, COLONY_H) * 0.44;
    var target = Math.min(1.15, Math.max(0.24, fitRadius / maxColonyRadius));
    s.cameraScale += (target - s.cameraScale) * Math.min(1, dt * 7);
    return s;
  }

  // One bacillus, with the central constriction (septum) while dividing
  function drawBacillus(ctx, cell) {
    var r = BASE_RADIUS;
    var halfL = (BASE_CYL_LEN + (MAX_CYL_LEN - BASE_CYL_LEN) * cell.progress) * 0.5;
    var pinch = 0;
    if (cell.progress > 0.52) {
      pinch = (cell.progress - 0.52) / 0.48;
      pinch = pinch * pinch * (3 - 2 * pinch);
    }
    var waistR = r * (1 - pinch * 0.78);

    ctx.save();
    ctx.translate(cell.x, cell.y);
    ctx.rotate(cell.angle);
    ctx.fillStyle = PALETTE[cell.colorIndex];
    ctx.strokeStyle = STROKE_COLOR;
    ctx.lineWidth = 2;
    ctx.lineJoin = 'round';
    ctx.lineCap = 'round';

    ctx.beginPath();
    if (pinch <= 0.01) {
      ctx.arc(-halfL, 0, r, Math.PI * 0.5, Math.PI * 1.5);
      ctx.lineTo(halfL, -r);
      ctx.arc(halfL, 0, r, -Math.PI * 0.5, Math.PI * 0.5);
    } else {
      var ps = Math.min(halfL * 0.65, r * 1.15);
      ctx.arc(-halfL, 0, r, Math.PI * 0.5, Math.PI * 1.5);
      ctx.lineTo(-ps, -r);
      ctx.bezierCurveTo(-ps * 0.35, -r, -ps * 0.25, -waistR, 0, -waistR);
      ctx.bezierCurveTo(ps * 0.25, -waistR, ps * 0.35, -r, ps, -r);
      ctx.lineTo(halfL, -r);
      ctx.arc(halfL, 0, r, -Math.PI * 0.5, Math.PI * 0.5);
      ctx.lineTo(ps, r);
      ctx.bezierCurveTo(ps * 0.35, r, ps * 0.25, waistR, 0, waistR);
      ctx.bezierCurveTo(-ps * 0.25, waistR, -ps * 0.35, r, -ps, r);
    }
    ctx.closePath();
    ctx.fill();
    ctx.stroke();

    // Septum line before separating
    if (pinch > 0.35) {
      ctx.beginPath();
      ctx.moveTo(0, -waistR);
      ctx.lineTo(0, waistR);
      ctx.strokeStyle = 'rgba(26, 26, 26, ' + (pinch - 0.35) * 1.2 + ')';
      ctx.lineWidth = 1.6;
      ctx.stroke();
    }

    // Subtle highlight for volume
    ctx.beginPath();
    ctx.moveTo(-halfL * 0.6, -r * 0.45);
    ctx.lineTo(halfL * 0.6, -r * 0.45);
    ctx.strokeStyle = 'rgba(255, 255, 255, 0.24)';
    ctx.lineWidth = 2.2;
    ctx.stroke();

    ctx.restore();
  }

  // Opacity of each blinking dot (keyframes 0-20% -> 0.12, 45-100% -> 1)
  function dotAlpha(now, delay) {
    var t = (((now / 1000 - delay) % 1.1) + 1.1) % 1.1 / 1.1;
    if (t <= 0.2) return 0.12;
    if (t >= 0.45) return 1;
    return 0.12 + (t - 0.2) / 0.25 * 0.88;
  }

  // "Working..." in white with a dark outline and offset shadow
  function drawLabel(ctx, now) {
    ctx.font = "56px Impact, 'Arial Black', 'Franklin Gothic Heavy', sans-serif";
    ctx.textAlign = 'left';
    ctx.textBaseline = 'middle';
    ctx.lineJoin = 'round';
    var parts = ['Working', '.', '.', '.'];
    var spacing = 2;
    var widths = parts.map(function (p) { return ctx.measureText(p).width; });
    var total = widths.reduce(function (a, b) { return a + b; }, 0) + spacing * (parts.length - 1);
    var x = (W - total) / 2;

    for (var i = 0; i < parts.length; i++) {
      ctx.globalAlpha = i === 0 ? 1 : dotAlpha(now, (i - 1) * 0.22);
      // shadow, outline and fill
      ctx.fillStyle = '#262626';
      ctx.fillText(parts[i], x + 3, LABEL_Y + 3);
      ctx.strokeStyle = '#262626';
      ctx.lineWidth = 7;
      ctx.strokeText(parts[i], x, LABEL_Y);
      ctx.fillStyle = '#FFFFFF';
      ctx.fillText(parts[i], x, LABEL_Y);
      x += widths[i] + spacing;
    }
    ctx.globalAlpha = 1;
  }

  function render(canvas, s, now) {
    var dpr = window.devicePixelRatio || 1;
    if (canvas.width !== Math.round(W * dpr)) {
      canvas.width = Math.round(W * dpr);
      canvas.height = Math.round(H * dpr);
    }
    var ctx = canvas.getContext('2d');
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, W, H);

    ctx.save();
    ctx.translate(W * 0.5, COLONY_H * 0.5);
    ctx.scale(s.cameraScale, s.cameraScale);
    for (var i = 0; i < s.cells.length; i++) drawBacillus(ctx, s.cells[i]);
    ctx.restore();

    drawLabel(ctx, now);
  }

  function frame(now) {
    var canvases = document.querySelectorAll('canvas.bacilli-loader');
    for (var i = 0; i < canvases.length; i++) {
      var canvas = canvases[i];
      // offsetParent is null when the canvas or an ancestor is display:none
      if (canvas.offsetParent === null) {
        states.delete(canvas); // restart from 2 cells next time it appears
        continue;
      }
      var s = states.get(canvas) || newState();
      var dt = Math.min(0.05, (now - s.lastTime) / 1000);
      s = update(s, dt);
      s.lastTime = now;
      states.set(canvas, s);
      render(canvas, s, now);
    }
    requestAnimationFrame(frame);
  }
  requestAnimationFrame(frame);
})();
