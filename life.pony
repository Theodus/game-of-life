use "collections"
use p = "collections/persistent"
use "itertools"

primitive Life
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
