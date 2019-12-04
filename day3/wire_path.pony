use "files"
use collections = "collections"

primitive Up
primitive Down
primitive Left
primitive Right

type Direction is (Up | Down | Left | Right)

class Point
  var _x : I32 = 0
  var _y : I32 = 0

  new create(x: I32, y: I32) =>
    _x = x
    _y = y

  new from_string(s: String) =>
    try
      let c = s.split(",")
      _x = c(0)?.i32()?
      _y = c(1)?.i32()?
    end

  fun to_string(): String =>
    _x.string() + "," + _y.string()

  fun manhattan(): I32 =>
    (_x.abs() + _y.abs()).i32()

  fun ref move(dir: Direction) =>
    match dir
    | Up => _y = _y + 1
    | Down => _y = _y - 1
    | Right => _x = _x + 1
    | Left => _x = _x - 1
    end

actor SharedMap
  let _data: collections.Map[String, (I32, I32)] = _data.create()
  var _nearest_distance: I32 = 2147483647
  var _shortest_wires: I32 = 2147483647

  be access(fn: {(SharedMap ref)} val) =>
    fn(this)

  be write(key: String, value: (I32, I32)) =>
    write_now(key, value)

  be read(key: String, fn: {((I32, I32))} val) =>
    fn(read_now(key))

  fun ref write_now(key: String, value: (I32, I32)) =>
    _data(key) = value

  fun ref read_now(key: String): (I32, I32) =>
    try _data(key)? else (0, 0) end

  fun ref update_nearest(x: I32) =>
    _nearest_distance = _nearest_distance.min(x)

  fun ref update_shortest(x: I32) =>
    _shortest_wires = _shortest_wires.min(x)

  fun get_nearest(): I32 =>
    _nearest_distance

  fun get_shortest(): I32 =>
    _shortest_wires

actor WirePuller
  let _map: SharedMap
  let _out: OutStream
  let _id: I32
  let _position: Point
  var _length: I32

  new create(map: SharedMap, out: OutStream, id: I32) =>
    _map = map
    _out = out
    _id = id
    _position = Point(0, 0)
    _length = 0

  fun ref move(dir: Direction) =>
    let map = _map
    let out = _out

    _position.move(dir)
    _length = _length + 1

    let key = _position.to_string()
    let distance = _position.manhattan()
    map.access({(map: SharedMap ref)(out, key) =>
      let newVal = match map.read_now(key)
      | (0, let l: I32) =>
        (_id, _length)
      | (_id, let l: I32) =>
        (_id, l)
      | (let id: I32, let l: I32) =>
        // out.print("P" + _id.string() + " crossed at " + key)
        if distance < map.get_nearest() then
          out.print("P" + _id.string() + " found new closest " + distance.string() + " at " + key)
          map.update_nearest(distance)
        end
        var length_both = l + _length
        if length_both < map.get_shortest() then
          out.print("P" + _id.string() + " found new shortest " + _length.string() + " + " + l.string() + " = " + length_both.string() + " at " + key)
          map.update_shortest(length_both)
        end
        (-1, l + _length)
      end
      map.write_now(key, newVal)
    } val)

  be move_many(command: String) =>
    let out = _out
    let map = _map
    try
      var steps = command.substring(1).i32()?
      let dir: Direction = match command.substring(0, 1)
      | "U" => Up
      | "D" => Down
      | "R" => Right
      | "L" => Left
      else
        steps = 0
        Up
      end
      var count: I32 = 0
      while count < steps do
        move(dir)
        count = count + 1
      end
    end

actor Main
  new create(env: Env) =>
    let fieldmap = SharedMap
    let out = env.out

    try
      var puller_id: I32 = 1
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        for line in file.lines() do
          let puller = WirePuller(fieldmap, out, puller_id)
          for command in line.split(",").values() do
            puller.move_many(command)
          end
          puller_id = puller_id + 1
        end
      end
    end
