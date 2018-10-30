use "collections"
use p ="collections/persistent"
use "itertools"
use "random"

interface tag GridNotify
  be update(idx: USize, live: Bool)
  be tick_done(cycle: Bool)

actor Grid
  let _notify: GridNotify
  let _rows: USize
  let _cols: USize
  var _cells: p.Vec[(Cell, Bool)]
  var _update_count: USize = 0
  let _hist_max: USize = 10
  embed _hist: Array[p.Vec[(Cell, Bool)]]
  embed _rand: Rand

  new create(columns: USize, rows: USize, seed: U64, notify: GridNotify) =>
    _notify = notify
    _cols = columns
    _rows = rows
    _cells = p.Vec[(Cell, Bool)]
    _hist = Array[p.Vec[(Cell, Bool)]](_hist_max)
    _rand = Rand(seed)

    reset()

  be start_tick() =>
    try _cells(0)?._1.start() end

  be reset() =>
    _hist.clear()
    _cells = p.Vec[(Cell, Bool)]
    for i in Range(0, _rows * _cols) do
      let live = _rand.int[U8](4) == 1
      let cell = Cell(live, i, this)
      _cells = _cells.push((cell, live))
    end
    _add_neighbors()

  fun ref _add_neighbors() =>
    for (idx, (cell, live)) in _cells.pairs() do
      let x = (idx % _cols, idx / _cols)
      let idxs =
        [ north(west((x))); north(x); north(east(x))
          west(x); east(x)
          south(west(x)); south(x); south(east(x))
        ]

      let count =
        Iter[(USize, USize)](idxs.values())
          .map[USize]({(c_r) => c_r._1 + (c_r._2 * _cols) })
          .map[Cell]({(i) ? => _cells(i)?._1 })
          .map[None]({(c) => cell.add_neighbor(c) })
          .count()

      _update_count = _update_count - (count - 1)
    end

  be _update(cell: Cell, idx: USize, live: Bool) =>
    _update_count = _update_count + 1
    try _cells = _cells.update(idx, (cell, live))? end
    _notify.update(idx, live)

    if _update_count == _cells.size() then
      _update_count = 0

      let cycle = _detect_cycle()
      if not cycle then
        if _hist.size() == _hist_max then try _hist.pop()? end end
        _hist.unshift(_cells)
      end
      _notify.tick_done(cycle)
    end

  fun _detect_cycle(): Bool =>
    let self: Grid = this
    let cells =
      {(g: p.Vec[(Cell, Bool)]): Iter[Bool] =>
        Iter[(Cell, Bool)](g.values()).map[Bool]({(c_l) => c_l._2 })
      }
    let grid_eq =
      {(prev: p.Vec[(Cell, Bool)]): Bool =>
        self.grid_cells(_cells)
          .zip[Bool](self.grid_cells(prev))
          .all({(cc) => cc._1 == cc._2 })
      }

    Iter[p.Vec[(Cell, Bool)]](_hist.values()).any(grid_eq)

  fun tag grid_cells(grid: p.Vec[(Cell, Bool)]): Iter[Bool]^ =>
    Iter[(Cell, Bool)](grid.values())
      .map[Bool]({(c_l) => c_l._2 })

  fun west(l: (USize, USize)): (USize, USize) =>
    (mod_dec(l._1, _cols), l._2)

  fun east(l: (USize, USize)): (USize, USize) =>
    (mod_inc(l._1, _cols), l._2)

  fun north(l: (USize, USize)): (USize, USize) =>
    (l._1, mod_dec(l._2, _rows))

  fun south(l: (USize, USize)): (USize, USize) =>
    (l._1, mod_inc(l._2, _rows))

  fun mod_inc(n: USize, m: USize): USize =>
    if n == (m - 1) then 0 else n + 1 end

  fun mod_dec(n: USize, m: USize): USize =>
    if n == 0 then (m - 1) else n - 1 end
