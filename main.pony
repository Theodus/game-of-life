use "collections"
use p = "collections/persistent"
use "itertools"
use "random"
use "time"

// TODO: cl arguments
// TODO: ANSI term size as default
// TODO: non-square grid
// TODO: wrap around on edges

actor Main
  let _env: Env
  var _cols: USize = 40

  var _grid: p.Vec[(Cell, Bool)] = p.Vec[(Cell, Bool)]
  var _update_count: USize = 0

  let _hist_max: USize = 5
  embed _hist: Array[p.Vec[(Cell, Bool)]]

  var _freq_hz: U64 = 10
  let _timers: Timers = Timers

  new create(env: Env) =>
    _env = env
    _hist = Array[p.Vec[(Cell, Bool)]](_hist_max)
    reset(Time.millis())

  fun ref reset(seed: U64) =>
    _hist.clear()
    _grid = p.Vec[(Cell, Bool)]
    let rand = Rand(seed)
    for i in Range(0, _cols * _cols) do
      let live = rand.int[U8](4) == 1
      let cell = Cell(live, i, this)
      _grid = _grid.push((cell, live))
    end
    try add_neighbors()? end

  fun ref add_neighbors() ? =>
    let idxs = Array[USize].init(0, 9)
    for (idx, (cell, live)) in _grid.pairs() do
      for i in Range(0, 9) do idxs(i)? = idx end
      for i in Range(0, 3) do idxs(i)? = (idxs(i)? - _cols) end
      for i in Range(6, 9) do idxs(i)? = (idxs(i)? + _cols) end
      for i in Range(0, 9, 3) do idxs(i)? = (idxs(i)? - 1) end
      for i in Range(2, 9, 3) do idxs(i)? = (idxs(i)? + 1) end

      let unset =
        {ref(r: Range) ? => for i in r do idxs(i)? = -1 end }
      if idx < _cols then unset(Range(0, 3))? end
      if idx >= (_grid.size() - _cols) then unset(Range(6, 9))? end
      if (idx % _cols) == 0 then unset(Range(0, 9, 3))? end
      if (idx % _cols) == (_cols - 1) then unset(Range(2, 9, 3))? end
      idxs(4)? = -1

      let n = Iter[USize](idxs.values())
        .filter({(i) => i != -1 })
        .map[Cell]({(i) ? => _grid(i)?._1 })
        .map[None]({(c) => cell.add_neighbor(c) })
        .count()

      // TODO: don't rely on underflow
      _update_count = _update_count - (n - 1)
    end

  be update(cell: Cell, idx: USize, live: Bool) =>
    _update_count = _update_count + 1
    try _grid = _grid.update(idx, (cell, live))? end

    if _update_count == _grid.size() then
      _update_count = 0
      _env.out.print(show_grid(_grid, _cols))

      if done() then
        reset(Time.millis())
      else
        if _hist.size() == _hist_max then try _hist.pop()? end end
        _hist.unshift(_grid)
        start_timer()
      end
    end

  fun done(): Bool =>
    let self: Main = this
    let cells =
      {(g: p.Vec[(Cell, Bool)]): Iter[Bool] =>
        Iter[(Cell, Bool)](g.values()).map[Bool]({(c_l) => c_l._2 })
      }
    let grid_eq =
      {(prev: p.Vec[(Cell, Bool)]): Bool =>
        self.grid_cells(_grid)
          .zip[Bool](self.grid_cells(prev))
          .all({(cc) => cc._1 == cc._2 })
      }

    Iter[p.Vec[(Cell, Bool)]](_hist.values()).any(grid_eq)

  fun ref start_timer() =>
    let notify =
      object iso is TimerNotify
        let self: Main = this
        fun apply(t: Timer, c: U64): Bool =>
          self.tick()
          false
      end
    _timers(Timer(consume notify, 1_000_000_000 / _freq_hz))

  be tick() =>
    try _grid(0)?._1.start() end

  fun tag show_grid(grid: p.Vec[(Cell, Bool)], cols: USize): String iso^ =>
    let show_cell =
      {(live: Bool): String => if live then "▀" else " " end }
    let insert_nl =
      {(i: USize): String => if (i % cols) == (cols - 1) then "\n" else "" end }
    let append_nl =
      {(i_s: (USize, String)): String =>
        " ".join([i_s._2; insert_nl(i_s._1)].values())
      }

    "".join(grid_cells(grid)
      .map[String](show_cell)
      .enum()
      .map[String](append_nl))

  fun tag grid_cells(grid: p.Vec[(Cell, Bool)]): Iter[Bool]^ =>
    Iter[(Cell, Bool)](grid.values())
      .map[Bool]({(c_l) => c_l._2 })
