use "collections"
use p = "collections/persistent"
use "itertools"
use "random"
use "time"

// TODO: non-square grid
// TODO: ANSI term size as default
// TODO: actor per cell, for fun

actor Main
  let _env: Env
  var _cols: USize = 20
  var _grid: p.Vec[Bool] = p.Vec[Bool]

  let _hist_max: USize = 3
  var _hist: Array[p.Vec[Bool]] = []

  var _freq_hz: U64 = 10
  let _timers: Timers = Timers
  let _timer: Timer tag

  new create(env: Env) =>
    _env = consume env
    if _env.args.size() > 1 then
      try _cols = _env.args(1)?.read_int[USize]()?._1 end
    end
    if _env.args.size() > 2 then
      try _freq_hz = _env.args(2)?.read_int[U64]()?._1 end
    end

    let notify =
      object iso is TimerNotify
        let self: Main = this
        fun apply(t: Timer, c: U64): Bool =>
          self.tick()
          true
      end
    let t = 1_000_000_000 / _freq_hz
    let timer = Timer(consume notify, t, t)
    _timer = timer
    _timers(consume timer)

    reset(Time.millis())
    print_grid()

  fun ref reset(seed: U64) =>
    _hist.clear()
    let rand = Rand(seed)
    _grid = _grid.concat(Iter[USize](Range(0, _cols * _cols))
      .map_stateful[Bool]({(n) => rand.int[U8](4) == 1 }))

  be tick() =>
    let grid' = try Life.grid_transition(_grid, _cols)? else _grid end

    if _hist.size() == _hist_max then try _hist.shift()? end end
    _hist.push(_grid)
    _grid = grid'

    if done() then
      // _timers.cancel(_timer)
      reset(Time.millis())
    end

    print_grid()

  fun done(): Bool =>
    let current = Iter[Bool](_grid.values())
    for grid in _hist.values() do
      let g = Iter[Bool](grid.values())
      if Life.grid_eq(current, g) then return true end
    end
    false

  fun print_grid() =>
    _env.out.print(Life.show_grid(Iter[Bool](_grid.values()), _cols))
