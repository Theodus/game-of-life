use "collections"
use "itertools"

actor Cell
  let _idx: USize
  let _grid: Grid
  var _live: Bool
  embed _neighbors: Array[Cell]
  embed _responses: MapIs[Cell, Bool]
  var _listening: Bool = false

  new create(live: Bool, idx: USize, grid: Grid) =>
    _idx = idx
    _grid = grid
    _live = live
    _neighbors = Array[Cell](8)
    _responses = MapIs[Cell, Bool](8)

  be add_neighbor(cell: Cell) =>
    _neighbors.push(cell)
    _grid._update(this, _idx, _live)

  be start() =>
    send_interactions()

  fun ref send_interactions() =>
    _listening = true
    let self: Cell = this
    Iter[Cell](_neighbors.values())
      .map[None]({(n) => n.interact(self, _live) })
      .run()

  be interact(cell: Cell, live: Bool) =>
    _responses(cell) = live
    if not _listening then send_interactions() end

    if _responses.size() == _neighbors.size() then
      let n =
        Iter[Bool](_responses.values())
          .filter({(b) => b })
          .count()

      _live = cell_transition(_live, n)
      _grid._update(this, _idx, _live)

      _responses.clear()
      _listening = false
    end

  fun tag cell_transition(live: Bool, neighbors: USize): Bool =>
    match (live, neighbors)
    | (true, 2) | (true, 3) | (false, 3) => true
    else false
    end
