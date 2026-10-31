/* Margin of Error | The 2026 Senate Stress Test: the browser-side app.
 *
 * The page layout comes from app.R's UI, rendered once to static HTML by
 * R/build_static.R. This file does the work the Shiny server used to do: it reads
 * the controls, computes each scenario, and fills in the outputs (horseshoe, map,
 * ranked chart, year strip, bias sliders, history tab, poll lists). Functions are
 * named after their R counterparts in app.R, and keep the same rules.
 */
(function () {
  'use strict';

  // ---- Constants (mirroring app.R) ------------------------------------------------
  var C = {
    SURFACE: '#fbf8f1', INK: '#1b1a17', INK_2: '#55524b', INK_MUTED: '#8a867c', GRID: '#e4e1da',
    NOT_UP: '#e8e6e2', D_DARK: '#1b4a76', D_MID: '#316ca5', D_TINT: '#e7f1fc',
    R_DARK: '#792b13', R_MID: '#ac4323', R_TINT: '#feebe6', I_DARK: '#2f6f66', I_MID: '#3f8a80'
  };
  var FONT = "'Newsreader', Georgia, serif";
  // Bins 1-8 run D to R; 9-10 are races an independent leads.
  var BINS = [null,
    { col: '#1b4a76', ink: '#ffffff' }, { col: '#386ca0', ink: '#ffffff' }, { col: '#6b90b6', ink: C.INK },
    { col: '#a8b9cc', ink: C.INK }, { col: '#ccb0a8', ink: C.INK }, { col: '#b97a67', ink: C.INK },
    { col: '#a44a30', ink: '#ffffff' }, { col: '#792b13', ink: '#ffffff' },
    { col: '#2f6f66', ink: '#ffffff' }, { col: '#8fbfb3', ink: C.INK }];
  var CYCLES = [2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024];
  var MIDTERMS = [2010, 2014, 2018, 2022], PRES_YEARS = [2012, 2016, 2020, 2024];
  var TRUMP_ERA = [2016, 2018, 2020, 2022, 2024], TRUMP_MID = [2018, 2022];
  var SEATS_NOT_UP = { D: 34, R: 31 };
  var NEED_FOR_CONTROL = 51 - SEATS_NOT_UP.D;
  var CONTROL_LABEL = { d: 'Democrats control', r: 'Republicans control', split: 'Split control' };
  var CONTROL_COL = { d: C.D_DARK, r: C.R_DARK, split: C.INK };
  var VIOLIN_Z = 1.959963984540054, CORE_Z = 0.6744897501960817, VIOLIN_W = 0.42;
  var VIOLIN_FILL = {
    outer: { D: 'rgba(49,108,165,0.26)', R: 'rgba(172,67,35,0.26)', I: 'rgba(63,138,128,0.26)' },
    core: { D: 'rgba(49,108,165,0.70)', R: 'rgba(172,67,35,0.70)', I: 'rgba(63,138,128,0.70)' },
    edge: { D: 'rgba(49,108,165,0.80)', R: 'rgba(172,67,35,0.80)', I: 'rgba(63,138,128,0.80)' }
  };
  var OFFSHORE_LABELS = [
    { abb: 'NH', lon: -71.55, lat: 43.40, lab_lon: -68.2, lab_lat: 43.10 },
    { abb: 'MA', lon: -71.90, lat: 42.35, lab_lon: -68.6, lab_lat: 42.00 },
    { abb: 'RI', lon: -71.52, lat: 41.68, lab_lon: -69.4, lab_lat: 40.90 },
    { abb: 'NJ', lon: -74.45, lat: 40.00, lab_lon: -71.2, lab_lat: 39.50 },
    { abb: 'DE', lon: -75.45, lat: 39.00, lab_lon: -72.6, lab_lat: 38.30 }];
  var MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];

  // ---- Small helpers ----------------------------------------------------------------
  var $ = window.jQuery;
  var D = null;            // data.json
  var byId = function (id) { return document.getElementById(id); };
  var isNum = function (x) { return typeof x === 'number' && !isNaN(x); };
  var f1 = function (x) { return (Math.round(x * 10) / 10).toFixed(1); };
  var mean = function (a) { var s = 0; for (var i = 0; i < a.length; i++) s += a[i]; return a.length ? s / a.length : NaN; };
  var sd = function (a) { var m = mean(a), s = 0; a.forEach(function (v) { s += (v - m) * (v - m); }); return Math.sqrt(s / (a.length - 1)); };
  var setEq = function (a, b) {
    if (a.length !== b.length) return false;
    var s = {}; a.forEach(function (v) { s[v] = 1; });
    return b.every(function (v) { return s[v]; });
  };
  var esc = function (s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  };
  var fmtTs = function (ts, withYear) {   // "2026-10-30 10:01" -> "Oct 30, 10:01" (or "Oct 30, 2026 at 10:01")
    var m = /^(\d{4})-(\d{2})-(\d{2}) (\d{2}:\d{2})/.exec(ts || '');
    if (!m) return ts || '';
    var md = MONTHS[+m[2] - 1] + ' ' + m[3];
    return withYear ? md + ', ' + m[1] + ' at ' + m[4] : md + ', ' + m[4];
  };
  var fmtShortDate = function (iso) { var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(iso || ''); return m ? MONTHS[+m[2] - 1] + ' ' + m[3] : ''; };

  // ---- Formatting (app.R: fmt_margin, fmt_error, fmt_miss, fmt_shift, ...) ---------------
  var fmtMargin = function (m, party) {
    if (!isNum(m)) return 'n/a';
    if (Math.abs(m) < 0.05) return 'Even';
    return m > 0 ? (party === 'I' ? 'Ind' : 'D') + '+' + f1(m) : 'R+' + f1(-m);
  };
  var fmtError = function (e) {
    if (!isNum(e)) return 'n/a';
    if (Math.abs(e) < 0.05) return '0.0';
    return e > 0 ? f1(e) + ' toward R' : f1(-e) + ' toward D';
  };
  var fmtMiss = function (e) {
    if (Math.abs(e) < 0.05) return '0';
    return '+' + f1(Math.abs(e)).replace(/\.0$/, '') + (e > 0 ? ' R' : ' D');
  };
  var fmtShift = function (x) {   // x > 0 = toward D (the model's convention)
    if (Math.abs(x) < 0.05) return '0';
    return '+' + f1(Math.abs(x)).replace(/\.0$/, '') + (x < 0 ? ' R' : ' D');
  };
  var marginBin = function (m, party) {
    if (m > 0 && party === 'I') return m >= 5 ? 9 : 10;
    var cuts = [-10, -5, -2, 0, 2, 5, 10];   // (-Inf,-10] -> 8 ... (10, Inf) -> 1
    var k = 0; while (k < cuts.length && m > cuts[k]) k++;
    return 8 - k;
  };
  var partyCol = function (m) { return Math.abs(m) < 0.05 ? C.INK : (m > 0 ? C.D_MID : C.R_MID); };
  var dotCol = function (m) { return Math.abs(m) < 0.05 ? C.INK_MUTED : partyCol(m); };
  var marginAxisLabels = function (v) { return v.map(function (x) { return x === 0 ? 'Even' : (x > 0 ? 'D+' + x : 'R+' + (-x)); }); };

  // Miss color ramp for the history grid (app.R: miss_fill): blue - gray - red, saturating at 12.
  var RAMP = ['#1b4a76', '#6b90b6', '#ebe8e2', '#b97a67', '#792b13'].map(function (h) {
    return [parseInt(h.substr(1, 2), 16), parseInt(h.substr(3, 2), 16), parseInt(h.substr(5, 2), 16)];
  });
  var missFill = function (e) {
    var t = (Math.max(Math.min(e, 12), -12) + 12) / 24 * (RAMP.length - 1);
    var i = Math.min(Math.floor(t), RAMP.length - 2), f = t - i;
    var c = RAMP[i].map(function (v, j) { return Math.round(v + (RAMP[i + 1][j] - v) * f); });
    return '#' + c.map(function (v) { return ('0' + v.toString(16)).slice(-2); }).join('');
  };

  // ---- State: the controls, read from the page -----------------------------------------
  var S = { custom: {}, poll_n: 60 };
  var checked = function (name) {
    return Array.prototype.map.call(document.querySelectorAll('input[name="' + name + '"]:checked'), function (el) { return el.value; });
  };
  var readInputs = function () {
    S.miss_mode = checked('miss_mode')[0] || 'none';
    S.cycles = checked('cycles').map(Number);
    S.types = checked('types');
    var sl = $('#shift').data('ionRangeSlider');
    S.shift = sl ? Number(sl.result.from) : Number(byId('shift').value || 0);
    S.ind_with_d = !!(byId('ind_with_d') && byId('ind_with_d').checked);
    S.seat_view = checked('seat_view')[0] || 'horseshoe';
    S.hm_view = checked('hm_view')[0] || 'rows';
    S.hm_type = checked('hm_type')[0] || 'Both';
    S.hm_years = checked('hm_years').map(Number);
    S.hm_2026_only = !!(byId('hm_2026_only') && byId('hm_2026_only').checked);
    S.avg_state = (byId('avg_state') && byId('avg_state').value) || 'all';
    S.lp_state = (byId('lp_state') && byId('lp_state').value) || 'all';
    S.lp_pollster = (byId('lp_pollster') && byId('lp_pollster').value) || 'all';
    S.lp_in_avg = !!(byId('lp_in_avg') && byId('lp_in_avg').checked);
  };
  var setChecks = function (name, values) {
    var want = {}; values.forEach(function (v) { want[String(v)] = 1; });
    document.querySelectorAll('input[name="' + name + '"]').forEach(function (el) { el.checked = !!want[el.value]; });
  };
  var selCycles = function () { return S.miss_mode === 'past' ? S.cycles : []; };
  var shiftD = function () { return -S.shift; };   // slider runs D (left) to R (right); the model counts toward D as positive
  var isCustom = function () { return Object.keys(S.custom).length > 0; };

  // ---- Scenario logic (app.R: assumed_errors, build_races, seat_totals, control_side) ----
  var assumedErrors = function (cycles, types) {
    var out = {};
    if (!cycles.length || !types.length) {
      D.seats.forEach(function (s) { out[s.abb] = 0; });
      return out;
    }
    var sel = D.errors.filter(function (e) { return cycles.indexOf(e.cycle) >= 0 && types.indexOf(e.race_type) >= 0; });
    var natl = sel.length ? mean(sel.map(function (e) { return e.error; })) : 0;
    var byState = {};
    sel.forEach(function (e) { (byState[e.abb] = byState[e.abb] || []).push(e.error); });
    D.seats.forEach(function (s) { out[s.abb] = byState[s.abb] ? mean(byState[s.abb]) : natl; });
    return out;
  };
  var pollsByAbb = {};
  var buildRaces = function (err, shift) {
    return D.seats.map(function (s) {
      var p = pollsByAbb[s.abb] || {};
      var r = {
        abb: s.abb, state: s.state, incumbent_party: s.incumbent_party,
        poll_margin: isNum(p.poll_margin) ? p.poll_margin : NaN,
        opp_name: p.opp_name, opp_party: p.opp_party, rep_name: p.rep_name,
        n_polls_in_avg: p.n_polls_in_avg, latest_poll: p.latest_poll,
        err: err[s.abb], custom_move: S.custom.hasOwnProperty(s.abb) ? S.custom[s.abb] : NaN
      };
      r.polled = isNum(r.poll_margin);
      // custom_move: a bias the reader set with the state's slider (> 0 = toward Republicans)
      r.proj_margin = r.polled ? r.poll_margin - (isNum(r.custom_move) ? r.custom_move : r.err - shift) : NaN;
      r.winner = !r.polled ? s.incumbent_party : (r.proj_margin > 0 ? (r.opp_party === 'I' ? 'I' : 'D') : 'R');
      r.flip = r.winner !== s.incumbent_party;
      r.bin = r.polled ? marginBin(r.proj_margin, r.opp_party) : (s.incumbent_party === 'R' ? 8 : 1);
      return r;
    });
  };
  var seatTotals = function (races) {
    var t = { D: SEATS_NOT_UP.D, I: 0, R: SEATS_NOT_UP.R };
    races.forEach(function (r) { t[r.winner]++; });
    return t;
  };
  var controlSide = function (t, indWithD) {
    var dem = t.D + (indWithD ? t.I : 0);
    return t.R >= 50 ? 'r' : (dem >= 51 ? 'd' : 'split');
  };
  // Seat order: unpolled D holds, polled races most D to most R, unpolled R holds.
  var raceOrder = function (races) {
    var key = function (r) { return r.polled ? r.proj_margin : (r.incumbent_party === 'D' ? Infinity : -Infinity); };
    var o = races.slice().sort(function (a, b) {
      var ka = key(a), kb = key(b);
      if (ka !== kb) return kb > ka ? 1 : -1;
      return a.abb < b.abb ? -1 : 1;
    });
    o.forEach(function (r) {
      r.tip = r.polled
        ? r.state + '\n' + r.opp_name + ' (' + r.opp_party + ') v ' + r.rep_name + ' (R)\nPoll avg ' + fmtMargin(r.poll_margin, r.opp_party) +
          ', assumed miss ' + fmtError(r.err) + '\nProjected ' + fmtMargin(r.proj_margin, r.opp_party)
        : r.state + '\nNo general-election polls\nAssumed safe ' + r.incumbent_party + ' hold';
    });
    return o;
  };
  var missDetail = function (abb, cycles, types, err, shift) {
    var shiftTxt = Math.abs(shift) >= 0.05 ? '\nPlus uniform shift: ' + fmtShift(shift) : '';
    if (!cycles.length || !types.length) return 'Polls taken at face value' + shiftTxt;
    var g = D.errors.filter(function (e) { return e.abb === abb && cycles.indexOf(e.cycle) >= 0 && types.indexOf(e.race_type) >= 0; })
      .sort(function (a, b) { return a.cycle - b.cycle || (a.race_type < b.race_type ? -1 : 1); });
    if (!g.length) return 'No polled race here in these years; uses the national average miss (' + fmtMiss(err) + ')' + shiftTxt;
    var lines = g.map(function (e) {
      return e.cycle + ' ' + (e.race_type === 'Senate' ? 'Senate' : 'president') + ': polls ' + fmtMargin(e.poll_margin) +
        ', result ' + fmtMargin(e.actual_margin) + ' (' + fmtMiss(e.error) + ' vs. polls)';
    });
    return lines.join('\n') + (g.length > 1 ? '\nAverage: ' + fmtMiss(err) : '') + shiftTxt;
  };
  var scenarioTag = function (cycles, types) {
    if (!cycles.length || !types.length) return 'Polls as published';
    var yrs = cycles.slice().sort();
    if (yrs.length === 1) return yrs[0] + ' polling error';
    if (setEq(yrs, MIDTERMS)) return 'Midterm-year polling error';
    if (setEq(yrs, PRES_YEARS)) return 'Presidential-year polling error';
    if (setEq(yrs, TRUMP_ERA)) return 'Trump-era polling error (2016-2024)';
    if (setEq(yrs, TRUMP_MID)) return 'Trump-era midterm polling error (2018, 2022)';
    return 'Mean polling error, ' + yrs.join(', ');
  };
  var scenarioCaption = function (cycles, types) {
    if (!cycles.length || !types.length) return 'Each race is projected at its current polling average as published, with no polling-error adjustment.';
    var kinds = ['Senate', 'President'].filter(function (t) { return types.indexOf(t) >= 0; })
      .map(function (t) { return t === 'Senate' ? 'Senate' : 'presidential'; }).join(' and ');
    var yrs = cycles.slice().sort();
    var yrTxt = yrs.length === 1 ? String(yrs[0]) : yrs.slice(0, -1).join(', ') + ' and ' + yrs[yrs.length - 1];
    var what = yrs.length === 1
      ? "Each state's polling average is shifted by its " + yrTxt + ' polling error in ' + kinds + ' races.'
      : "Each state's polling average is shifted by its mean polling error in " + kinds + ' races across ' + yrTxt + '.';
    return what + ' States with no polled race in those years take the national average error.';
  };
  var partyIconSvg = function (side, size) {
    if (!D.party_icon[side]) return '';
    return '<svg class="party-icon" viewBox="0 0 640 512" style="height:' + size + ';width:auto" aria-hidden="true"><path fill="' +
      CONTROL_COL[side] + '" d="' + D.party_icon[side] + '"/></svg>';
  };

  // ---- Horseshoe (app.R: HS, hs_point, hs_band, horseshoe_svg) --------------------------
  var HS = { w: 1100, h: 300, x0: 30, xc: 790, ymid: 150, r: 118, t: 40, gap: 2.5, race: 38 };
  HS.arm = HS.xc - HS.x0;
  HS.len = 2 * HS.arm + Math.PI * HS.r;
  var hsPoint = function (s) {
    if (s <= HS.arm) return [HS.x0 + s, HS.ymid - HS.r, 0, -1];
    if (s <= HS.arm + Math.PI * HS.r) {
      var th = -Math.PI / 2 + (s - HS.arm) / HS.r;
      return [HS.xc + HS.r * Math.cos(th), HS.ymid + HS.r * Math.sin(th), Math.cos(th), Math.sin(th)];
    }
    return [HS.xc - (s - HS.arm - Math.PI * HS.r), HS.ymid + HS.r, 0, 1];
  };
  var hsBand = function (s0, s1, t) {
    t = t || HS.t;
    var n = Math.max(2, Math.ceil((s1 - s0) / 5)), p = [];
    for (var i = 0; i < n; i++) p.push(hsPoint(s0 + (s1 - s0) * i / (n - 1)));
    var outer = p.map(function (q) { return [q[0] + t / 2 * q[2], q[1] + t / 2 * q[3]]; });
    var inner = p.map(function (q) { return [q[0] - t / 2 * q[2], q[1] - t / 2 * q[3]]; }).reverse();
    return 'M' + outer.concat(inner).map(function (q) { return f1(q[0]) + ',' + f1(q[1]); }).join('L') + 'Z';
  };
  var horseshoeSvg = function (ordered, totals, indWithD) {
    var nd = SEATS_NOT_UP.D, nr = SEATS_NOT_UP.R, w = HS.race, half = HS.len / 2;
    var raceStart = function (i) { return half - (50 - nd) * w + (i - 1) * w; };
    var block = function (s0, s1, outer, inner, fill, label) {
      var band = hsBand(s0, s1), b = hsPoint(outer);
      var y0 = b[1] - HS.t / 2 - 4, y1 = b[1] + HS.t / 2 + 4, k = 5, mid = hsPoint((outer + inner) / 2);
      return '<path d="' + band + '" fill="' + fill + '"/><path d="' + band + '" fill="url(#hs-hatch)"/>' +
        '<path class="hs-break-gap" d="M' + f1(b[0] - 3.5 + k) + ',' + f1(y0) + 'L' + f1(b[0] + 3.5 + k) + ',' + f1(y0) + 'L' +
        f1(b[0] + 3.5 - k) + ',' + f1(y1) + 'L' + f1(b[0] - 3.5 - k) + ',' + f1(y1) + 'Z"/>' +
        '<line class="hs-break" x1="' + f1(b[0] - 3.5 + k) + '" y1="' + f1(y0) + '" x2="' + f1(b[0] - 3.5 - k) + '" y2="' + f1(y1) + '"/>' +
        '<line class="hs-break" x1="' + f1(b[0] + 3.5 + k) + '" y1="' + f1(y0) + '" x2="' + f1(b[0] + 3.5 - k) + '" y2="' + f1(y1) + '"/>' +
        '<text class="hs-chunk-label" x="' + f1(mid[0]) + '" y="' + f1(mid[1]) + '">' + label + '</text>';
    };
    var cells = ordered.map(function (r, j) {
      var i = j + 1, s0 = raceStart(i) + HS.gap / 2, s1 = raceStart(i + 1) - HS.gap / 2, mid = hsPoint((s0 + s1) / 2);
      var bin = BINS[r.bin], tip = r.tip;
      if (i === NEED_FOR_CONTROL) tip += '\nThe 51st seat: whoever wins it controls the Senate';
      if (r.flip) tip += '\nFlips party in this scenario';
      var ring = r.flip ? '<circle class="hs-flip" cx="' + f1(mid[0]) + '" cy="' + f1(mid[1]) + '" r="13" stroke="' + bin.ink + '"/>' : '';
      var hatch = r.bin >= 9 ? '<path d="' + hsBand(s0, s1) + '" fill="url(#hs-ihatch)" pointer-events="none"/>' : '';
      return '<path class="hs-cell" d="' + hsBand(s0, s1) + '" fill="' + bin.col + '" data-tip="' + esc(tip) + '"/>' + hatch + ring +
        '<text class="hs-cell-label" x="' + f1(mid[0]) + '" y="' + f1(mid[1]) + '" fill="' + bin.ink + '">' + r.abb + '</text>';
    }).join('');
    var apex = hsPoint(half);
    var e0 = [apex[0] - (HS.t / 2 + 8) * apex[2], apex[1] - (HS.t / 2 + 8) * apex[3]];
    var e1 = [apex[0] + (HS.t / 2 + 12) * apex[2], apex[1] + (HS.t / 2 + 12) * apex[3]];
    var lab = [apex[0] + (HS.t / 2 + 17) * apex[2], apex[1] + (HS.t / 2 + 17) * apex[3] - 9];
    var k0 = raceStart(NEED_FOR_CONTROL), k1 = raceStart(NEED_FOR_CONTROL + 1), km = hsPoint((k0 + k1) / 2);
    var keyLab = [km[0] + (HS.t / 2 + 26) * km[2], km[1] + (HS.t / 2 + 26) * km[3]];
    var cx = 500;
    var tally = totals.I > 0
      ? '<text class="hs-tally" x="' + cx + '" y="' + (HS.ymid - 8) + '"><tspan fill="' + C.D_DARK + '">' + totals.D + '</tspan>' +
        '<tspan fill="' + C.INK_MUTED + '" dx="12">–</tspan><tspan fill="' + C.I_DARK + '" dx="12">' + totals.I + '</tspan>' +
        '<tspan fill="' + C.INK_MUTED + '" dx="12">–</tspan><tspan fill="' + C.R_DARK + '" dx="12">' + totals.R + '</tspan></text>'
      : '<text class="hs-tally" x="' + cx + '" y="' + (HS.ymid - 8) + '"><tspan fill="' + C.D_DARK + '">' + totals.D + '</tspan>' +
        '<tspan fill="' + C.INK_MUTED + '" dx="14">–</tspan><tspan fill="' + C.R_DARK + '" dx="14">' + totals.R + '</tspan></text>';
    var side = controlSide(totals, indWithD), ctl = CONTROL_LABEL[side];
    var iconW = 30, halfW = ctl.length * 4.9, sh = D.party_icon[side] ? (iconW + 6) / 2 : 0;
    var ctlSvg = (sh > 0 ? '<svg x="' + f1(cx + sh - halfW - iconW - 6) + '" y="' + f1(HS.ymid + 34 - 18) + '" width="' + iconW +
        '" height="24" viewBox="0 0 640 512"><path fill="' + CONTROL_COL[side] + '" d="' + D.party_icon[side] + '"/></svg>' : '') +
      '<text class="hs-tally-ctl" x="' + f1(cx + sh) + '" y="' + (HS.ymid + 34) + '" fill="' + CONTROL_COL[side] + '">' + ctl + '</text>';
    return '<svg class="hs" viewBox="0 0 ' + HS.w + ' ' + HS.h + '" role="img" aria-label="Senate seats: ' + totals.D +
      ' Democratic caucus, ' + totals.I + ' independent, ' + totals.R + ' Republican">' +
      '<defs><pattern id="hs-hatch" width="7" height="7" patternUnits="userSpaceOnUse" patternTransform="rotate(45)">' +
      '<line x1="0" y1="0" x2="0" y2="7" stroke="#ffffff" stroke-opacity="0.16" stroke-width="2.5"/></pattern>' +
      '<pattern id="hs-ihatch" width="5" height="5" patternUnits="userSpaceOnUse" patternTransform="rotate(-45)">' +
      '<line x1="0" y1="0" x2="0" y2="5" stroke="#1b1a17" stroke-opacity="0.22" stroke-width="1.2"/></pattern></defs>' +
      '<path class="hs-key" d="' + hsBand(k0, k1, HS.t + 36) + '"/>' +
      block(0, raceStart(1) - HS.gap / 2, 34, raceStart(1), C.D_DARK, nd + ' Dem-caucus seats not up') +
      cells +
      block(raceStart(ordered.length + 1) + HS.gap / 2, HS.len, HS.len - 34, raceStart(ordered.length + 1), C.R_DARK, nr + ' Republican seats not up') +
      '<text class="hs-key-label" x="' + f1(keyLab[0]) + '" y="' + f1(keyLab[1] + 2) + '"><tspan font-weight="600">51st seat</tspan>' +
      '<tspan x="' + f1(keyLab[0]) + '" dy="15"> decides control</tspan></text>' +
      '<line class="hs-ctl" x1="' + f1(e0[0]) + '" y1="' + f1(e0[1]) + '" x2="' + f1(e1[0]) + '" y2="' + f1(e1[1]) + '"/>' +
      '<text class="hs-ctl-label" x="' + f1(lab[0]) + '" y="' + f1(lab[1]) + '">50 seats</text>' +
      tally + ctlSvg + '</svg>';
  };

  // ---- Plotly helpers -----------------------------------------------------------------
  var plotLayout = function (extra) {
    var base = {
      font: { family: FONT, color: C.INK_2, size: 13 },
      paper_bgcolor: 'rgba(0,0,0,0)', plot_bgcolor: 'rgba(0,0,0,0)',
      hoverlabel: { bgcolor: '#ffffff', bordercolor: C.GRID, font: { family: FONT, color: C.INK } }
    };
    for (var k in extra) base[k] = extra[k];
    return base;
  };
  var PLOT_CONFIG = { displayModeBar: false, responsive: true };
  var visible = function (el) { return el && el.offsetParent !== null; };

  // ---- Ranked margins (app.R: output$ranked, violin_parts) ----------------------------------
  var violinParts = function (x, m, s, cap, z) {
    var ys = [];
    for (var i = 0; i < 31; i++) ys.push(m - z * s + (2 * z * s) * i / 30);
    ys = ys.filter(function (y) { return y >= -cap && y <= cap; });
    if (m - z * s < 0 && m + z * s > 0) { ys.push(0); ys.sort(function (a, b) { return a - b; }); }
    var piece = function (keep) {
      var y = ys.filter(keep);
      if (y.length < 2) return null;
      var wv = y.map(function (v) { return VIOLIN_W * Math.exp(-(v - m) * (v - m) / (2 * s * s)); });
      return { x: wv.map(function (w) { return x - w; }).concat(wv.slice().reverse().map(function (w) { return x + w; })),
               y: y.concat(y.slice().reverse()) };
    };
    return { D: piece(function (v) { return v >= 0; }), R: piece(function (v) { return v <= 0; }) };
  };
  var drawRanked = function (ordered) {
    var el = byId('ranked');
    if (!visible(el)) return;
    var r = ordered, s = sd(D.errors.map(function (e) { return e.error; })), cap = 30;
    var clip = function (v) { return Math.max(Math.min(v, cap), -cap); };
    var x = r.map(function (_, i) { return i + 1; });
    var traces = [];
    ['outer', 'core'].forEach(function (layer) {
      var acc = { D: { x: [], y: [] }, I: { x: [], y: [] }, R: { x: [], y: [] } };
      r.forEach(function (q, i) {
        if (!q.polled) return;
        var pt = violinParts(x[i], q.poll_margin, s, cap, layer === 'outer' ? VIOLIN_Z : CORE_Z);
        var parts = { D: q.opp_party !== 'I' ? pt.D : null, I: q.opp_party === 'I' ? pt.D : null, R: pt.R };
        ['D', 'I', 'R'].forEach(function (side) {
          if (!parts[side]) return;
          acc[side].x = acc[side].x.concat(parts[side].x, [null]);
          acc[side].y = acc[side].y.concat(parts[side].y, [null]);
        });
      });
      ['D', 'I', 'R'].forEach(function (side) {
        if (!acc[side].x.length) return;
        traces.push({ x: acc[side].x, y: acc[side].y, type: 'scatter', mode: 'lines', fill: 'toself',
          fillcolor: VIOLIN_FILL[layer][side], hoverinfo: 'skip',
          line: layer === 'outer' ? { width: 0.8, color: VIOLIN_FILL.edge[side] } : { width: 0 } });
      });
    });
    var hover = function (q) {
      return '<b>' + q.state + '</b><br>' + q.opp_name + ' (' + q.opp_party + ') v ' + q.rep_name + ' (R)<br>Polling average: <b>' +
        fmtMargin(q.poll_margin, q.opp_party) + '</b><br>Range of past misses (95%): ' + fmtMargin(q.poll_margin - VIOLIN_Z * s, q.opp_party) +
        ' to ' + fmtMargin(q.poll_margin + VIOLIN_Z * s, q.opp_party) + '<br>This scenario: ' + fmtMargin(q.proj_margin, q.opp_party) +
        ' (assumed miss ' + fmtError(q.err) + ')';
    };
    var px = [], lo = [], hi = [], ht = [], ctr = [], m = [], fx = [];
    r.forEach(function (q, i) {
      if (!q.polled) return;
      if (q.flip) fx.push(x[i]);
      var l = clip(Math.min(q.poll_margin - VIOLIN_Z * s, q.proj_margin)), h = clip(Math.max(q.poll_margin + VIOLIN_Z * s, q.proj_margin));
      px.push(x[i]); lo.push(l); hi.push(h - l); ht.push(hover(q)); ctr.push(clip(q.poll_margin)); m.push(clip(q.proj_margin));
    });
    traces.push({ x: px, y: hi, base: lo, type: 'bar', width: 0.9, marker: { color: 'rgba(0,0,0,0)' }, text: ht,
      hoverinfo: 'text', textposition: 'none' });
    traces.push({ x: px, y: ctr, type: 'scatter', mode: 'markers', text: ht, hoverinfo: 'text',
      marker: { symbol: 'line-ew', size: 22, line: { color: C.INK, width: 3 } } });
    traces.push({ x: px, y: m, type: 'scatter', mode: 'markers', text: ht, hoverinfo: 'text',
      marker: { symbol: 'circle', size: 8, color: '#ffffff', line: { color: C.INK, width: 1.5 } } });
    traces.push({ x: fx, y: fx.map(function () { return -cap + 1.8; }), type: 'scatter', mode: 'markers', hoverinfo: 'skip',
      marker: { symbol: 'circle-open', size: 15, color: C.INK, line: { width: 1.3 } } });
    var k51 = NEED_FOR_CONTROL;
    var shapes = [
      { type: 'rect', xref: 'paper', x0: 0, x1: 1, y0: 0, y1: cap + 1, fillcolor: C.D_TINT, opacity: 0.7, line: { width: 0 }, layer: 'below' },
      { type: 'rect', xref: 'paper', x0: 0, x1: 1, y0: -cap - 1, y1: 0, fillcolor: C.R_TINT, opacity: 0.7, line: { width: 0 }, layer: 'below' },
      { type: 'rect', x0: k51 - 0.5, x1: k51 + 0.5, yref: 'paper', y0: 0, y1: 1, fillcolor: 'rgba(192,138,30,0.12)', line: { width: 0 }, layer: 'below' },
      { type: 'line', x0: k51 - 0.5, x1: k51 - 0.5, yref: 'paper', y0: 0, y1: 1, line: { color: C.INK, width: 2 } }];
    var notes = [
      { x: k51 - 0.5, y: 1, yref: 'paper', xanchor: 'right', yanchor: 'bottom', showarrow: false, text: '50 seats ', font: { size: 12, color: C.INK } },
      { x: k51 - 0.5, y: 1, yref: 'paper', xanchor: 'left', yanchor: 'bottom', showarrow: false, text: ' <b>51st seat</b> decides control', font: { size: 12, color: C.INK } }];
    ['D', 'R'].forEach(function (side) {
      var idx = []; r.forEach(function (q, i) { if (!q.polled && q.incumbent_party === side) idx.push(x[i]); });
      if (!idx.length) return;
      var a = Math.min.apply(null, idx), b = Math.max.apply(null, idx);
      shapes.push({ type: 'rect', x0: a - 0.5, x1: b + 0.5, yref: 'paper', y0: 0, y1: 1, fillcolor: C.NOT_UP, opacity: 0.95, line: { width: 0 }, layer: 'below' });
      notes.push({ x: (a + b) / 2, y: 0.5, yref: 'paper', showarrow: false, textangle: -90, text: 'Safe ' + side + ' (no polls)',
        font: { size: 12, color: side === 'D' ? C.D_MID : C.R_MID } });
    });
    var ticks = [-30, -20, -10, 0, 10, 20, 30];
    Plotly.react(el, traces, plotLayout({
      barmode: 'overlay', showlegend: false, shapes: shapes, annotations: notes,
      margin: { t: 28, l: 56, r: 10, b: 30 },
      xaxis: { tickvals: x, ticktext: r.map(function (q, i) { return i + 1 === k51 ? '<b>' + q.abb + '</b>' : q.abb; }),
        range: [0.4, r.length + 0.6], showgrid: false, tickfont: { size: 11 }, color: C.INK_2, fixedrange: true },
      yaxis: { range: [-cap - 1, cap + 1], tickvals: ticks, ticktext: marginAxisLabels(ticks), gridcolor: '#ffffff', gridwidth: 1.5,
        zeroline: true, zerolinecolor: C.INK, zerolinewidth: 2, color: C.INK_MUTED, fixedrange: true }
    }), PLOT_CONFIG);
  };

  // ---- Map (app.R: output$map) ----------------------------------------------------------
  var drawMap = function (races) {
    var el = byId('map');
    if (!visible(el)) return;
    var cyc = selCycles();
    var stateName = {}; D.states.forEach(function (s) { stateName[s.abb] = s.name; });
    var up = {}; races.forEach(function (r) { up[r.abb] = r; });
    var notUp = D.states.map(function (s) { return s.abb; }).filter(function (a) { return !up[a]; });
    var traces = [{ type: 'choropleth', locationmode: 'USA-states', locations: notUp, z: notUp.map(function () { return 1; }),
      colorscale: [[0, C.NOT_UP], [1, C.NOT_UP]], showscale: false, marker: { line: { color: C.SURFACE, width: 1 } },
      text: notUp.map(function (a) { return '<b>' + stateName[a] + '</b><br>No Senate race in 2026'; }), hoverinfo: 'text' }];
    var hover = function (r) {
      if (!r.polled) return '<b>' + r.state + '</b><br>No general-election polling<br>Assumed safe ' + r.incumbent_party + ' hold';
      var detail = missDetail(r.abb, cyc, S.types, r.err, 0).replace(/\n/g, '<br>');
      return '<b>' + r.state + '</b>' + (r.flip ? ' (flip)' : '') + '<br>' + r.opp_name + ' (' + r.opp_party + ') vs ' + r.rep_name +
        ' (R)<br>Poll avg: ' + fmtMargin(r.poll_margin, r.opp_party) + '  (' + r.n_polls_in_avg + ' poll' + (r.n_polls_in_avg === 1 ? '' : 's') +
        ', latest ' + fmtShortDate(r.latest_poll) + ')<br>Assumed miss: ' + fmtError(r.err) + '<br><i>' + detail +
        '</i><br><b>Projected: ' + fmtMargin(r.proj_margin, r.opp_party) + '</b>';
    };
    var bins = {}; races.forEach(function (r) { (bins[r.bin] = bins[r.bin] || []).push(r); });
    Object.keys(bins).map(Number).sort(function (a, b) { return a - b; }).forEach(function (b) {
      traces.push({ type: 'choropleth', locationmode: 'USA-states', locations: bins[b].map(function (r) { return r.abb; }),
        z: bins[b].map(function () { return 1; }), colorscale: [[0, BINS[b].col], [1, BINS[b].col]], showscale: false,
        marker: { line: { color: C.SURFACE, width: 1 } }, text: bins[b].map(hover), hoverinfo: 'text' });
    });
    var offAbbs = OFFSHORE_LABELS.map(function (o) { return o.abb; });
    var centers = D.states.filter(function (s) { return up[s.abb] && offAbbs.indexOf(s.abb) < 0; }).map(function (s) {
      var c = { abb: s.abb, lon: s.lon, lat: s.lat };
      if (s.abb === 'MI') { c.lon = -84.6; c.lat = 43.3; }
      if (s.abb === 'FL') c.lon = -81.6;
      if (s.abb === 'AK') { c.lon = -152.5; c.lat = 64.2; }
      return c;
    });
    var off = OFFSHORE_LABELS.filter(function (o) { return up[o.abb]; });
    var rings = centers.filter(function (c) { return up[c.abb].flip; }).map(function (c) { return { lon: c.lon, lat: c.lat, col: BINS[up[c.abb].bin].ink }; })
      .concat(off.filter(function (o) { return up[o.abb].flip; }).map(function (o) { return { lon: o.lab_lon + 0.75, lat: o.lab_lat, col: C.INK }; }));
    if (rings.length) {
      traces.push({ type: 'scattergeo', mode: 'markers', lon: rings.map(function (q) { return q.lon; }), lat: rings.map(function (q) { return q.lat; }),
        marker: { symbol: 'circle-open', size: 26, color: rings.map(function (q) { return q.col; }), line: { width: 1.3 } }, hoverinfo: 'skip', showlegend: false });
    }
    var leadLon = [], leadLat = [];
    off.forEach(function (o) { leadLon.push(o.lon, o.lab_lon - 0.25, null); leadLat.push(o.lat, o.lab_lat, null); });
    traces.push({ type: 'scattergeo', mode: 'text', lon: centers.map(function (c) { return c.lon; }), lat: centers.map(function (c) { return c.lat; }),
      text: centers.map(function (c) { return c.abb; }), hoverinfo: 'skip', showlegend: false,
      textfont: { size: 11, color: centers.map(function (c) { return BINS[up[c.abb].bin].ink; }), family: FONT } });
    traces.push({ type: 'scattergeo', mode: 'lines', lon: leadLon, lat: leadLat, line: { color: C.INK_MUTED, width: 1 }, hoverinfo: 'skip', showlegend: false });
    traces.push({ type: 'scattergeo', mode: 'text', lon: off.map(function (o) { return o.lab_lon; }), lat: off.map(function (o) { return o.lab_lat; }),
      text: off.map(function (o) { return o.abb; }), textposition: 'middle right', hoverinfo: 'skip', showlegend: false,
      textfont: { size: 11, color: C.INK, family: FONT } });
    Plotly.react(el, traces, plotLayout({
      geo: { scope: 'usa', projection: { type: 'albers usa' }, showlakes: false, bgcolor: 'rgba(0,0,0,0)' },
      showlegend: false, dragmode: false, margin: { l: 0, r: 0, t: 0, b: 0 }
    }), { displayModeBar: false, responsive: true, scrollZoom: false, doubleClick: false });
  };

  // ---- Scenario Explorer outputs ------------------------------------------------------
  var setHtml = function (id, html) { var el = byId(id); if (el) el.innerHTML = html; };
  var setText = function (id, txt) { var el = byId(id); if (el) el.textContent = txt; };

  var renderScenario = function () {
    var cyc = selCycles(), sh = shiftD();
    var races = buildRaces(assumedErrors(cyc, S.types), sh);
    var totals = seatTotals(races), ordered = raceOrder(races);

    setText('scenario_caption', S.miss_mode === 'past' && !S.cycles.length ? 'Select at least one election year.'
      : (S.miss_mode === 'past' && !S.types.length ? 'Select Senate, presidential, or both.' : scenarioCaption(cyc, S.types)));
    setHtml('scenario_title', '<span class="t-tag scen-title-tag">' + esc(isCustom() ? 'Custom polling error'
      : scenarioTag(cyc, S.types) + (sh !== 0 ? ', shift ' + fmtShift(sh) : '')) + '</span>');

    setHtml('horseshoe', horseshoeSvg(ordered, totals, S.ind_with_d));
    var side = controlSide(totals, S.ind_with_d);
    setHtml('m_tally', '<div class="m-tally"><div class="m-nums"><span style="color:' + C.D_DARK + '">' + totals.D + '</span><span class="m-dash">–</span>' +
      (totals.I > 0 ? '<span style="color:' + C.I_DARK + '">' + totals.I + '</span><span class="m-dash">–</span>' : '') +
      '<span style="color:' + C.R_DARK + '">' + totals.R + '</span></div><div class="m-ctl" style="color:' + CONTROL_COL[side] + '">' +
      partyIconSvg(side, '1.1em') + CONTROL_LABEL[side] + '</div></div>');

    renderYearStrip();
    renderBiasList(races, cyc, sh);
    updateConditionals();
    // Phones: open the swipeable horseshoe at its right end (races and the 51st seat), once it is visible.
    var sc = document.querySelector('.seat-scroll');
    if (!S.scrolled && sc && visible(sc) && sc.scrollWidth > sc.clientWidth) { sc.scrollLeft = sc.scrollWidth; S.scrolled = true; }
    drawMap(races);
    drawRanked(ordered);
    // quick-pick button matching the selected years
    var cy = S.cycles, id = setEq(cy, MIDTERMS) ? 'cy_mid' : setEq(cy, PRES_YEARS) ? 'cy_pres' : setEq(cy, TRUMP_ERA) ? 'cy_trump'
      : setEq(cy, TRUMP_MID) ? 'cy_tmid' : setEq(cy, CYCLES) ? 'cy_all' : '';
    $('.preset').removeClass('active'); if (id) $('#' + id).addClass('active');
  };

  var renderYearStrip = function () {
    var types = S.types.length ? S.types : ['Senate', 'President'];
    var cyc = selCycles(), oneYear = S.miss_mode === 'past' && cyc.length === 1;
    var cells = CYCLES.map(function (y) {
      var t = seatTotals(buildRaces(assumedErrors([y], types), shiftD()));
      var side = controlSide(t, S.ind_with_d);
      return '<button type="button" class="yr-cell ' + side + (oneYear && cyc[0] === y ? ' active' : '') + '" title="If 2026\'s polls miss as ' + y +
        '\'s did: ' + t.D + ' Dem, ' + t.I + ' Ind, ' + t.R + ' Rep. ' + CONTROL_LABEL[side] + '." onclick="Shiny.setInputValue(\'yr_pick\', ' + y +
        ', {priority: \'event\'})"><span class="yr-y">' + y + '</span><span class="yr-s">' +
        (t.I > 0 ? t.D + '–' + t.I + '–' + t.R : t.D + '–' + t.R) + '</span></button>';
    }).join('');
    setHtml('year_strip', '<div class="yr-wrap"><div class="yr-title">Projected 2026 seats assuming each single year\'s polling error</div>' +
      '<div class="yr-strip">' + cells + '</div></div>');
  };

  var renderBiasList = function (races, cyc, sh) {
    var cap = 10, pos = function (v) { return 50 + 50 * Math.max(Math.min(v, cap), -cap) / cap; };
    var r = races.filter(function (q) { return q.polled; }).map(function (q) {
      var edited = isNum(q.custom_move);
      return { q: q, edited: edited, move: edited ? q.custom_move : q.err - sh };
    }).sort(function (a, b) { return a.move - b.move; });
    var nSet = r.filter(function (x) { return x.edited; }).length;
    var rows = r.map(function (x) {
      var v = x.move, col = dotCol(-v), lo = Math.min(pos(0), pos(v)), hi = Math.max(pos(0), pos(v));
      var title = x.edited ? x.q.state + ': you set this to ' + fmtMiss(v) + '. Double-click to reset.'
        : x.q.state + ': projection moved ' + fmtMiss(v) + ' from the polls\n' + missDetail(x.q.abb, cyc, S.types, x.q.err, sh) + '\nDrag to set your own.';
      return '<div class="bias-row' + (x.edited ? ' edited' : '') + '" data-abb="' + x.q.abb + '" title="' + esc(title) + '">' +
        '<span class="bias-st">' + x.q.abb + '</span><div class="bias-track"><span class="mid"></span>' +
        '<span class="line" style="left:' + f1(lo) + '%;width:' + f1(hi - lo) + '%;background:' + col + '"></span>' +
        '<span class="dot" style="left:' + f1(pos(v)) + '%;background:' + col + '"></span></div>' +
        '<span class="bias-v" style="color:' + col + '">' + fmtMiss(v) + '</span></div>';
    }).join('');
    setHtml('bias_list', '<div class="bias-tools"><span class="bias-hint">' +
      (nSet ? nSet + ' state' + (nSet === 1 ? '' : 's') + ' set by you' : "Drag a state's dot to set its own bias") + '</span>' +
      (nSet ? '<span class="bias-all"><a href="#" onclick="Shiny.setInputValue(\'bias_reset\', {abb: \'__all__\', t: Date.now()}); return false;">Reset all</a></span>' : '') +
      '</div><div class="bias-rows">' + rows + '</div>');
  };

  // conditionalPanel(...) visibility, evaluated against the current state.
  var updateConditionals = function () {
    var input = { miss_mode: S.miss_mode, shift: S.shift, seat_view: S.seat_view, hm_view: S.hm_view };
    var output = { bias_is_custom: isCustom() };
    document.querySelectorAll('[data-display-if]').forEach(function (el) {
      var show;
      try { show = !!(new Function('input', 'output', 'return (' + el.getAttribute('data-display-if') + ');'))(input, output); } catch (e) { show = true; }
      el.style.display = show ? 'block' : 'none';   // Shiny's stylesheet hides these until shown
    });
  };

  // ---- Historical Polling Misses (app.R: hm_err, hm_states, hm_rows, hm_grid) ------------
  var raceTip = function (g) {
    return g.cycle + ' ' + (g.race_type === 'Senate' ? 'Senate' : 'president') + ': polls ' + fmtMargin(g.poll_margin) + ', result ' +
      fmtMargin(g.actual_margin) + ' (' + (Math.abs(g.error) < 0.05 ? 'polls exact'
        : (g.error > 0 ? 'Republican' : 'Democrat') + ' beat the polls by ' + f1(Math.abs(g.error))) + ')';
  };
  var renderHistory = function () {
    var up = {}; D.seats.forEach(function (s) { up[s.abb] = 1; });
    var name = {}; D.states.forEach(function (s) { name[s.abb] = s.name; });
    var e = D.errors.filter(function (x) {
      return S.hm_years.indexOf(x.cycle) >= 0 && (S.hm_type === 'Both' || x.race_type === S.hm_type) && (!S.hm_2026_only || up[x.abb]);
    });
    var hid = setEq(S.hm_years, CYCLES) ? 'hm_all' : setEq(S.hm_years, MIDTERMS) ? 'hm_mid' : setEq(S.hm_years, PRES_YEARS) ? 'hm_pres'
      : setEq(S.hm_years, TRUMP_ERA) ? 'hm_trump' : setEq(S.hm_years, TRUMP_MID) ? 'hm_tmid' : '';
    $('.hm-preset').removeClass('active'); if (hid) $('#' + hid).addClass('active');
    updateConditionals();
    if (!e.length) {
      setHtml('hm_rows', '<p class="lp-unpolled">No races match these filters.</p>');
      setHtml('hm_grid', '<p class="lp-unpolled">No races match these filters.</p>');
      return;
    }
    var groups = {}; e.forEach(function (x) { (groups[x.abb] = groups[x.abb] || []).push(x); });
    var st = Object.keys(groups).map(function (a) {
      return { abb: a, state: name[a] || a, avg: mean(groups[a].map(function (x) { return x.error; })), n: groups[a].length };
    }).sort(function (a, b) { return b.avg - a.avg; });

    var cap = 16, pos = function (v) { return 50 + 50 * Math.max(Math.min(v, cap), -cap) / cap; };
    var header = '<div class="lp-cols hm-cols"><span>State</span><div class="av-axis">' + [-15, -10, -5, 0, 5, 10, 15].map(function (v) {
      return '<span' + (Math.abs(v) === 5 || Math.abs(v) === 15 ? ' class="minor"' : '') + ' style="left:' + f1(pos(v)) + '%">' +
        (v === 0 ? 'Even' : fmtMiss(v)) + '</span>';
    }).join('') + '</div><span>Average</span><span>Races</span></div>';
    var rows = st.map(function (s) {
      var g = groups[s.abb].slice().sort(function (a, b) { return a.cycle - b.cycle; });
      var dots = g.map(function (x, j) {
        var jit = 22 + 56 * (((j + 1) * 0.618034) % 1);
        return '<span class="dot ' + (x.race_type === 'Senate' ? 'sen' : 'pres') + '" style="left:' + f1(pos(x.error)) + '%;top:' +
          Math.round(jit) + '%;--c:' + dotCol(-x.error) + '" title="' + esc(raceTip(x)) + '"></span>';
      }).join('');
      var a = s.avg, col = dotCol(-a);
      return '<div class="hm-row"><span class="hm-st">' + esc(s.state) + '</span><div class="hm-track">' +
        '<span class="tick" style="left:' + f1(pos(-10)) + '%"></span><span class="tick" style="left:' + f1(pos(10)) + '%"></span>' +
        '<span class="fill" style="' + (a > 0 ? 'left' : 'right') + ':50%;width:' + f1(Math.abs(pos(a) - 50)) + '%;background:' + col + '"></span>' +
        '<span class="mid"></span>' + dots + '<span class="avg" style="left:' + f1(pos(a)) + '%;background:' + col + '"></span></div>' +
        '<span class="hm-avg" style="color:' + col + '">' + fmtMiss(a) + '</span><span class="hm-n">' + s.n + '</span></div>';
    }).join('');
    setHtml('hm_rows', header + rows);

    // grid: states down, elections across
    var colsSeen = {}, cols = [];
    e.forEach(function (x) { var k = x.cycle + '|' + x.race_type; if (!colsSeen[k]) { colsSeen[k] = 1; cols.push({ cycle: x.cycle, race_type: x.race_type }); } });
    cols.sort(function (a, b) { return a.cycle - b.cycle || ((a.race_type !== 'Senate') - (b.race_type !== 'Senate')); });
    var types = {}; cols.forEach(function (c) { types[c.race_type] = 1; });
    var twoRows = Object.keys(types).length > 1;
    var yrs = []; cols.forEach(function (c) { if (yrs.indexOf(c.cycle) < 0) yrs.push(c.cycle); });
    var head1 = '<tr><th class="st"></th>' + yrs.map(function (y) {
      return '<th class="yr" colspan="' + cols.filter(function (c) { return c.cycle === y; }).length + '">' + y + '</th>';
    }).join('') + '<th class="av">Average</th></tr>';
    var head2 = twoRows ? '<tr><th></th>' + cols.map(function (c) { return '<th class="ty">' + (c.race_type === 'Senate' ? 'Sen' : 'Pres') + '</th>'; }).join('') + '<th></th></tr>' : '';
    var body = st.map(function (s) {
      var cells = cols.map(function (c) {
        var hit = groups[s.abb].filter(function (x) { return x.cycle === c.cycle && x.race_type === c.race_type; })[0];
        if (!hit) return '<td class="empty"></td>';
        var v = hit.error;
        return '<td style="background:' + missFill(v) + ';color:' + (Math.abs(v) > 7 ? '#ffffff' : C.INK) + '" title="' + esc(raceTip(hit)) + '">' +
          (Math.abs(v) < 0.5 ? '0' : Math.round(Math.abs(v))) + '</td>';
      }).join('');
      return '<tr><td class="st">' + esc(s.state) + '</td>' + cells + '<td class="av" style="color:' + dotCol(-s.avg) + '">' + fmtMiss(s.avg) + '</td></tr>';
    }).join('');
    setHtml('hm_grid', '<div class="hm-legend"><span>' + fmtMiss(-12) + '</span><span class="ramp"></span><span>' + fmtMiss(12) +
      '</span><span class="hm-legend-note">Numbers are the miss in points; the color says who beat the polls.</span></div>' +
      '<div class="hm-grid-wrap"><table class="hm-table"><thead>' + head1 + head2 + '</thead><tbody>' + body + '</tbody></table></div>');
  };
  var natlDrawn = false;
  var drawNatlBias = function () {
    var el = byId('natl_bias');
    if (!visible(el)) return;
    var spec = typeof D.natl_bias === 'string' ? JSON.parse(D.natl_bias) : D.natl_bias;
    if (!natlDrawn) { Plotly.newPlot(el, spec.data, spec.layout, { displayModeBar: false, responsive: true }); natlDrawn = true; }
    else Plotly.Plots.resize(el);
  };

  // ---- Polling Averages and All Latest Polls -------------------------------------------
  var renderAverages = function () {
    var pick = S.avg_state === 'all' ? null : S.avg_state;
    var rows = D.avg_rows.filter(function (x) { return !pick || x.abb === pick; });
    var unpolled = D.seats.filter(function (s) { return !pollsByAbb[s.abb] && (!pick || s.abb === pick); });
    setHtml('avg_list', (rows.length ? D.avg_header + rows.map(function (x) { return x.html; }).join('') : '') +
      (unpolled.length ? '<p class="lp-unpolled"><b>No general-election polls</b> (treated as safe holds): ' +
        unpolled.map(function (s) { return esc(s.state) + ' (' + s.incumbent_party + ')'; }).join(', ') + '</p>' : ''));
  };
  var renderPolls = function () {
    var pl = D.poll_rows.filter(function (x) {
      return (S.lp_state === 'all' || x.abb === S.lp_state) && (S.lp_pollster === 'all' || x.pollster === S.lp_pollster) && (!S.lp_in_avg || x.in_average);
    });
    if (!pl.length) { setHtml('poll_list', '<p class="lp-unpolled">No polls match these filters.</p>'); setHtml('poll_more', ''); return; }
    var shown = pl.slice(0, S.poll_n), perDay = {};
    pl.forEach(function (x) { perDay[x.date] = (perDay[x.date] || 0) + 1; });
    var html = D.poll_header, last = null;
    shown.forEach(function (x) {
      if (x.date !== last) {
        last = x.date;
        html += '<div class="pr-day"><b>' + esc(x.day) + '</b><span>' + perDay[x.date] + ' poll' + (perDay[x.date] === 1 ? '' : 's') + '</span></div>';
      }
      html += x.html;
    });
    setHtml('poll_list', html);
    setHtml('poll_more', pl.length > S.poll_n
      ? '<button id="poll_more_btn" type="button" class="btn btn-default action-button btn-sm btn-outline-secondary">Show more polls (' +
        S.poll_n + ' of ' + pl.length + ' shown)</button>' : '');
  };

  // ---- Events ---------------------------------------------------------------------------
  var timer = null;
  var soon = function (fn) { clearTimeout(timer); timer = setTimeout(fn, 60); };
  var setSelect = function (id, v) {
    var el = byId(id); if (!el) return;
    if (el.selectize) el.selectize.setValue(v, true); else el.value = v;
  };

  // Messages from the page's buttons and scripts (in place of Shiny's server).
  window.App = {
    // For the parity check against R (R/check_static.R): one scenario's projections and seats.
    compute: function (cfg) {
      var saved = S.custom;
      S.custom = cfg.custom || {};
      var races = buildRaces(assumedErrors(cfg.cycles, cfg.types), cfg.shift_d);
      var t = seatTotals(races), proj = {};
      races.forEach(function (r) { proj[r.abb] = isNum(r.proj_margin) ? Math.round(r.proj_margin * 1e6) / 1e6 : null; });
      var out = { D: t.D, I: t.I, R: t.R, side: controlSide(t, !!cfg.ind_with_d), proj: proj };
      S.custom = saved;
      return out;
    },
    input: function (name, v) {
      if (name === 'bias_set') { S.custom[v.abb] = Number(v.v); renderScenario(); }
      else if (name === 'bias_reset') {
        if (v.abb === '__all__') S.custom = {}; else delete S.custom[v.abb];
        renderScenario();
      } else if (name === 'yr_pick') {
        setChecks('miss_mode', ['past']); setChecks('cycles', [v]); readInputs(); renderScenario();
      } else if (name === 'drill_state') {
        setSelect('lp_state', v); setSelect('lp_pollster', 'all'); S.poll_n = 60;
        var tab = $('.navbar .nav-link').filter(function () { return $(this).text().trim() === 'All Latest Polls'; });
        if (tab.length) tab[0].click();
        readInputs(); renderPolls();
      }
    }
  };

  var wire = function () {
    // sidebar and seat-view controls
    $(document).on('change', 'input[name=miss_mode], input[name=cycles], input[name=types], #ind_with_d, input[name=seat_view]', function () {
      readInputs(); renderScenario();
    });
    $('#shift').on('change', function () { readInputs(); soon(renderScenario); });
    var presets = { cy_mid: MIDTERMS, cy_pres: PRES_YEARS, cy_trump: TRUMP_ERA, cy_tmid: TRUMP_MID, cy_all: CYCLES };
    Object.keys(presets).forEach(function (id) {
      $('#' + id).on('click', function () { setChecks('cycles', presets[id]); readInputs(); renderScenario(); });
    });
    // history tab
    $(document).on('change', 'input[name=hm_view], input[name=hm_type], input[name=hm_years], #hm_2026_only', function () { readInputs(); renderHistory(); });
    var hmPresets = { hm_all: CYCLES, hm_mid: MIDTERMS, hm_pres: PRES_YEARS, hm_trump: TRUMP_ERA, hm_tmid: TRUMP_MID };
    Object.keys(hmPresets).forEach(function (id) {
      $('#' + id).on('click', function () { setChecks('hm_years', hmPresets[id]); readInputs(); renderHistory(); });
    });
    // lists
    $(document).on('change', '#avg_state', function () { readInputs(); renderAverages(); });
    $(document).on('change', '#lp_state, #lp_pollster, #lp_in_avg', function () { readInputs(); S.poll_n = 60; renderPolls(); });
    $(document).on('click', '#poll_more_btn', function () { S.poll_n += 60; renderPolls(); });
    // tabs: charts need to be visible to size themselves
    $(document).on('shown.bs.tab', function () {
      readInputs();
      drawNatlBias();
      renderScenario();
    });
  };

  var initControls = function () {
    // shift slider, with "+2 R" / "+4 D" labels
    $('#shift').ionRangeSlider({
      skin: 'shiny',
      prettify_enabled: true,
      prettify: function (n) { var a = Math.round(Math.abs(n) * 10) / 10; return a === 0 ? '0' : '+' + a + (n < 0 ? ' D' : ' R'); },
      onChange: function () { $('#shift').trigger('change'); }
    });
    // dropdowns
    var poll = byId('lp_pollster');
    if (poll) D.pollsters.forEach(function (p) { var o = document.createElement('option'); o.value = p; o.textContent = p; poll.appendChild(o); });
    ['avg_state', 'lp_state', 'lp_pollster'].forEach(function (id) {
      var el = byId(id);
      if (el && $.fn.selectize) $(el).selectize({});
    });
    // data-dependent text
    setText('freshness', 'Polls updated ' + fmtTs(D.fetched_at) + ' (refreshed daily)');
    setText('avg_updated', 'Updated ' + fmtTs(D.fetched_at, true));
    setText('lp_updated', 'Updated ' + fmtTs(D.fetched_at, true));
    setHtml('hm_tiles', D.hm_tiles);
    setHtml('ev_list', D.ev_html || '');
    setText('ev_updated', D.ev_updated || '');
    Object.keys(D.about).forEach(function (k) { setText(k, D.about[k]); });
    var s = sd(D.errors.map(function (e) { return e.error; }));
    var within = D.errors.filter(function (e) { return Math.abs(e.error) <= VIOLIN_Z * s; }).length / D.errors.length;
    setText('ranked_caption', '*±' + f1(VIOLIN_Z * s) + ' pts around each polling average; ' + Math.round(100 * within) +
      '% of single-race polling misses in 2010–2024 fell within it.');
  };

  var start = function () {
    D.polls.forEach(function (p) { pollsByAbb[p.abb] = p; });
    initControls();
    // Shiny's tab code adds Bootstrap's nav classes and marks the first tab active; do it here.
    document.querySelectorAll('.navbar a[data-bs-toggle="tab"]').forEach(function (a) {
      a.classList.add('nav-link');
      if (a.parentElement) a.parentElement.classList.add('nav-item');
    });
    var firstTab = document.querySelector('.navbar .nav-link[data-bs-toggle="tab"]');
    if (firstTab && !document.querySelector('.navbar .nav-link.active') && window.bootstrap) bootstrap.Tab.getOrCreateInstance(firstTab).show();
    wire();
    readInputs();
    renderScenario();
    renderHistory();
    renderAverages();
    renderPolls();
    // replay anything clicked before the data arrived
    (window.Shiny._q || []).forEach(function (q) { window.App.input(q[0], q[1]); });
    window.Shiny._q = [];
  };

  fetch('data.json', { cache: 'no-cache' }).then(function (r) { return r.json(); }).then(function (data) {
    D = data;
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
  });
})();
