use "collections"
use "files"
use "itertools"
use "promises"

class val Point is Comparable[Point]
  let _x: I32
  let _y: I32

  new create(x: I32, y: I32) =>
    _x = x
    _y = y

  fun string(): String =>
    _x.string() + "," + _y.string()

  fun add(other: Point): Point =>
    recover Point(_x + other._x, _y + other._y) end

  fun sub(other: Point): Point =>
    recover Point(_x - other._x, _y - other._y) end

  fun eq(other: Point): Bool =>
    (_x == other._x) and (_y == other._y)

  fun ne(other: Point): Bool =>
    (_x != other._x) or (_y != other._y)

  fun det(other: Point): I32 =>
    (_x * other._y) - (_y * other._x)

  fun dot(other: Point): I32 =>
    (_x * other._x) + (_y * other._y)

  fun phi(): F64 =>
    if (_x == 0) then
      if (_y < 0) then
        -F64.pi()
      else
        0.0
      end
    else
      (-_x).f64().atan2(_y.f64())
    end

  fun square(): I32 =>
    (_x * _x) + (_y * _y)

  fun lt(other: Point): Bool =>
    phi() < other.phi()

  fun le(other: Point): Bool =>
    this.lt(other) or this.eq(other)

  fun gt(other: Point): Bool =>
    phi() > other.phi()

  fun ge(other: Point): Bool =>
    this.gt(other) or this.eq(other)

  fun compare(other: Point): (Less val | Equal val | Greater val) =>
    if this.eq(other) then
      Equal
    else
      if this.lt(other) then
        Less
      else
        if this.gt(other) then
          Greater
        else
          Equal
        end
      end
    end

class val AsteroidSummary
  let _position: Point val
  let _can_see_count: USize

  new create(position: Point val, can_see_count: USize) =>
    _position = recover position end
    _can_see_count = can_see_count

  fun get_position() : Point =>
    _position

  fun get_can_see_count() : USize =>
    _can_see_count

actor AsteroidChecker
  let _out: OutStream
  let _position: Point
  let _can_see_asteroids: Array[Point]

  new create(out: OutStream, position: Point val) =>
    _out = out
    _position = recover position end
    _can_see_asteroids = []

  be check_positions(asteroid_positions: Array[Point] val) =>
    var i: USize = 0
    try
      while i < asteroid_positions.size() do
        var j: USize = 0
        if asteroid_positions(i)? == _position then
          i = i + 1
          continue
        end
        let adist = asteroid_positions(i)? - _position
        var shadowed: Bool = false
        while j < asteroid_positions.size() do
          if asteroid_positions(j)? == _position then
            j = j + 1
            continue
          end
          let bdist = asteroid_positions(j)? - _position
          if (adist.det(bdist) == 0) and (adist.dot(bdist) > 0) then
            if bdist.square() < adist.square() then
              shadowed = true
              break
            end
          end
          j = j + 1
        end
        if not shadowed then
          var can_see = asteroid_positions(i)?
          _can_see_asteroids.push(can_see)
        end
        i = i + 1
      end
      // _out.print(_position.string() + " can see " + _can_see_asteroids.size().string())
    end

  be get_summary(p: Promise[AsteroidSummary]) =>
    var can_see_num = _can_see_asteroids.size()
    p(recover AsteroidSummary(_position, can_see_num) end)

  be vaporize_visible() =>
    let visible_directions: Array[Point] = []
    for vpos in _can_see_asteroids.values() do
      visible_directions.push(vpos - _position)
    end
    let sorted_visible = Sort[Array[Point], Point](visible_directions)
    var i: USize = 1
    for a in sorted_visible.values() do
      _out.print(i.string() + ". asteroid to be vaporized is " + (a + _position).string() +
      " direction is " + a.string() + " angle is " + a.phi().string())
      i = i + 1
    end

actor Main
  let _out: OutStream
  let _positions_val: Array[Point] val

  new create(env: Env) =>
    _out = env.out
    let positions: Array[Point] trn = recover Array[Point] end
    try
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        var i: I32 = 0
        for line in file.lines() do
          let l: String ref = line.clone()
          var j: I32 = 0
          for c in l.values() do
            if c == '#' then
              positions.push(recover Point(j, i) end)
            end
            j = j + 1
          end
          i = i + 1
        end
      end
    end
    _positions_val = consume positions

    let create_summary_promise =
    {(position: Point): Promise[AsteroidSummary] =>
      let asteroid_checker = AsteroidChecker(_out, position)
      asteroid_checker.check_positions(_positions_val)
      let p = Promise[AsteroidSummary]
      asteroid_checker.get_summary(p)
      p
    } iso

    Promises[AsteroidSummary].join(
      Iter[Point val](_positions_val.values())
        .map[Promise[AsteroidSummary]](consume create_summary_promise))
      .next[None](recover this~receive_collection() end)

  be receive_collection(coll: Array[AsteroidSummary] val) =>
    _out.print("Received asteroid summaries:")
    var most: USize = 0
    var best_position: Point = recover Point(0, 0) end
    for summary in coll.values() do
      if summary.get_can_see_count() > most then
        most = summary.get_can_see_count()
        best_position = summary.get_position()
      end
    end
    _out.print(best_position.string() + " is best with " + most.string() + " detected")
    let best_checker = AsteroidChecker(_out, best_position)
    best_checker.check_positions(_positions_val)
    best_checker.vaporize_visible()
