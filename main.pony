use "collections"
use p = "collections/persistent"
use "itertools"
use "random"
use "time"

// TODO: cl arguments
// TODO: non-square grid
// TODO: ANSI term size as default
// TODO: actor per cell, for fun
// TODO: wrap around on edges

actor Main
  let _env: Env
  var _cols: USize = 20

  var _grid: p.Vec[(Cell, Bool)] = p.Vec[(Cell, Bool)]
  var _update_count: USize = 0

  let _hist_max: USize = 5
  embed _hist: Array[p.Vec[(Cell, Bool)]]

  var _freq_hz: U64 = 15
  let _timers: Timers = Timers

  new create(env: Env) =>
    _env = env
    _hist = Array[p.Vec[(Cell, Bool)]](_hist_max)
    reset(Time.millis())

  fun print_grid() =>
    _env.out.print(Life.show_grid(
      Iter[(Cell, Bool)](_grid.values()).map[Bool]({(cc) => cc._2 }),
      _cols))

  fun ref reset(seed: U64) =>
    _hist.clear()
    _grid = p.Vec[(Cell, Bool)]
    let rand = Rand(seed)
    for i in Range(0, _cols * _cols) do
      let live = rand.int[U8](3) == 1
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
      print_grid()

      if done() then
        reset(Time.millis())
      else
        if _hist.size() == _hist_max then try _hist.pop()? end end
        _hist.unshift(_grid)
        start_timer()
      end
    end

  fun done(): Bool =>
    for grid in _hist.values() do
      let second = {(c_l: (Cell, Bool)): Bool => c_l._2 }
      let current = Iter[(Cell, Bool)](_grid.values()).map[Bool](second)
      let prev = Iter[(Cell, Bool)](grid.values()).map[Bool](second)
      if Life.grid_eq(current, prev) then return true end
    end
    false

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

actor Cell
  let _idx: USize
  var _live: Bool
  embed _neighbors: Array[Cell]
  embed _responses: MapIs[Cell, Bool]
  var _listening: Bool = false

  let _notify: Main

  new create(live: Bool, idx: USize, notify: Main) =>
    _idx = idx
    _live = live
    _neighbors = Array[Cell](8)
    _responses = MapIs[Cell, Bool](8)
    _notify = notify

  be add_neighbor(cell: Cell) =>
    _neighbors.push(cell)
    _notify.update(this, _idx, _live)

  be reset(live: Bool) =>
    _live = live

  be start() =>
    send_interactions()

  fun ref send_interactions() =>
    for neighbor in _neighbors.values() do
      neighbor.interact(this, _live)
    end
    _listening = true

  be interact(cell: Cell, live: Bool) =>
    _responses(cell) = live
    if not _listening then
      send_interactions()
    end
    if _responses.size() == _neighbors.size() then
      let n = Iter[Bool](_responses.values())
        .filter({(b) => b })
        .count()
      _live = Life.cell_transition(_live, n)
      _notify.update(this, _idx, _live)

      _responses.clear()
      _listening = false
    end
