use "collections"
use p = "collections/persistent"
use "itertools"

primitive Life
  fun grid_transition(grid: p.Vec[Bool], cols: USize): p.Vec[Bool] ? =>
    var grid' = grid
    let idxs = Array[USize].init(0, 9)
    for (idx, live) in grid.pairs() do
      for i in Range(0, 9) do idxs(i)? = idx end
      for i in Range(0, 3) do idxs(i)? = (idxs(i)? - cols) end
      for i in Range(6, 9) do idxs(i)? = (idxs(i)? + cols) end
      for i in Range(0, 9, 3) do idxs(i)? = (idxs(i)? - 1) end
      for i in Range(2, 9, 3) do idxs(i)? = (idxs(i)? + 1) end

      let unset =
        {ref(r: Range) ? => for i in r do idxs(i)? = -1 end }
      if idx < cols then unset(Range(0, 3))? end
      if idx >= (grid.size() - cols) then unset(Range(6, 9))? end
      if (idx % cols) == 0 then unset(Range(0, 9, 3))? end
      if (idx % cols) == (cols - 1) then unset(Range(2, 9, 3))? end

      let neighbors =
        Iter[USize](idxs.values())
          .filter({(i) => (i < grid.size()) and (i != idx) })
          .map[USize]({(i) ? => if grid(i)? then 1 else 0 end })
          .fold[USize](0, {(a, b) => a + b })

      grid' = grid'.update(idx, Life.cell_transition(live, neighbors))?
    end
    grid'

  fun cell_transition(live: Bool, neighbors: USize): Bool =>
    match (live, neighbors)
    | (true, 2) | (true, 3) | (false, 3) => true
    else false
    end

  fun grid_eq(a: Iter[Bool], b: Iter[Bool]): Bool =>
    a.zip[Bool](b).all({(cc) => cc._1 == cc._2 })

  fun show_grid(grid: Iter[Bool], cols: USize): String iso^ =>
    let insert_nl =
      {(i: USize): String => if (i % cols) == (cols - 1) then "\n" else "" end }

    "".join(grid
      .map[String](Life~show_cell())
      .enum()
      .map[String]({(i_s) => " ".join([i_s._2; insert_nl(i_s._1)].values()) }))

  fun show_cell(live: Bool): String =>
    if live then "▀" else " " end
