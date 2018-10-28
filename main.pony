use "collections"
use p = "collections/persistent"
use "itertools"
use "random"
use "time"

actor Main
  let _env: Env
  var _dim: USize = 45
  var _cells: p.Vec[Bool] = p.Vec[Bool]
  var _prev: p.Vec[Bool] = p.Vec[Bool]

  let _timers: Timers = Timers
  let _timer: Timer tag

  new create(env: Env) =>
    _env = env

    let notify =
      object iso is TimerNotify
        let self: Main = this
        fun apply(t: Timer, c: U64): Bool =>
          self.tick()
          true
      end
    let timer = Timer(consume notify, 100_000_000, 100_000_000)
    _timer = timer
    _timers(consume timer)

    reseed(Time.millis())
    print_cells()

  be tick() =>
    let cells' = try life()? else _cells end
    if done(_cells, cells') or done(_prev, cells') then
      // _timers.cancel(_timer)
      reseed(Time.millis())
    else
      _prev = _cells = cells'
    end
    print_cells()

  fun ref reseed(seed: U64) =>
    let rand = Rand(seed)
    _cells = _cells.concat(Iter[USize](Range(0, _dim * _dim))
      .map_stateful[Bool]({(n) => rand.int[U8](2) == 1 }))

    _prev = _cells

  fun done(cells: p.Vec[Bool], cells': p.Vec[Bool]): Bool =>
    Iter[Bool](cells.values())
      .zip[Bool](cells'.values())
      .all({(cc) => cc._1 == cc._2 })

  fun life(): p.Vec[Bool] ? =>
    var cells' = _cells
    let idxs = Array[USize].init(0, 9)
    for (idx, l) in _cells.pairs() do
      for i in Range(0, 9) do idxs(i)? = idx end
      for i in Range(0, 3) do idxs(i)? = (idxs(i)? - _dim) end
      for i in Range(6, 9) do idxs(i)? = (idxs(i)? + _dim) end
      for i in Range(0, 9, 3) do idxs(i)? = (idxs(i)? - 1) end
      for i in Range(2, 9, 3) do idxs(i)? = (idxs(i)? + 1) end

      let unset =
        {ref(r: Range) ? => for i in r do idxs(i)? = -1 end }

      if idx < _dim then unset(Range(0, 3))? end
      if idx >= (_cells.size() - _dim) then unset(Range(6, 9))? end
      if (idx % _dim) == 0 then unset(Range(0, 9, 3))? end
      if (idx % _dim) == (_dim - 1) then unset(Range(2, 9, 3))? end

      let live_neighbors =
        Iter[USize](idxs.values())
          .filter({(i) => (i < _cells.size()) and (i != idx) })
          .map[USize]({(i) ? => if _cells(i)? then 1 else 0 end })
          .fold[USize](0, {(a, b) => a + b })

      cells' = cells'.update(idx, cell_life(_cells(idx)?, live_neighbors))?
    end
    cells'

  fun tag cell_life(live: Bool, neighbors_alive: USize): Bool =>
    match (live, neighbors_alive)
    | (true, 2) | (true, 3) | (false, 3) => true
    else false
    end

  fun print_cells() =>
    _env.err.print(show_cells(Iter[Bool](_cells.values())))
    _env.err.print("".join(
      Iter[USize](Range(0, _dim)).map[String]({(n) => "--"})))

  fun show_cells(cells: Iter[Bool]): String iso^ =>
    let ss = recover String end
    for (i, cell) in cells.enum() do
      ss.append(if cell then "▀ " else "  " end)
      if (i % _dim) == (_dim - 1) then ss.append("\n") end
    end
    try ss.pop()? end
    ss
