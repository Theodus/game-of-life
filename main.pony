use "itertools"
use "time"

// TODO: parse cl arguments
// TODO: ANSI term size as default

actor Main
  let _env: Env
  var _cols: USize = 10
  var _rows: USize = 10
  embed _board: Array[Bool]
  let _grid: Grid

  var _freq_hz: U64 = 10
  let _timers: Timers = Timers

  new create(env: Env) =>
    _env = env

    if env.args.size() > 2 then
      try
        _cols = env.args(1)?.read_int[USize]()?._1
        _rows = env.args(2)?.read_int[USize]()?._1
      end
    end
    if env.args.size() > 3 then
      try _freq_hz = env.args(3)?.read_int[U64]()?._1 end
    end

    _board = Array[Bool].init(false, _rows * _cols)
    _grid = Grid(_cols, _rows, Time.millis(), this)

  be update(idx: USize, live: Bool) =>
    try _board(idx)? = live end

  be tick_done(cycle: Bool) =>
    print_board()
    if cycle then
      _grid.reset()
    else
      start_timer()
    end

  fun ref start_timer() =>
    let notify =
      object iso is TimerNotify
        fun apply(t: Timer, c: U64): Bool =>
          _grid.start_tick()
          false
      end
    _timers(Timer(consume notify, 1_000_000_000 / _freq_hz))

  fun print_board() =>
    let show_cell =
      {(live: Bool): String => if live then "▀" else " " end }
    let insert_nl =
      {(i: USize): String =>
        if (i % _cols) == (_cols - 1) then "\n" else "" end
      }
    let append_nl =
      {(i_s: (USize, String)): String =>
        " ".join([i_s._2; insert_nl(i_s._1)].values())
      }

    _env.out.print(
      "".join(Iter[Bool](_board.values())
        .map[String](show_cell)
        .enum()
        .map[String](append_nl)))
