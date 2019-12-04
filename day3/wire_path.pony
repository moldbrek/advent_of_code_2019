use "files"
use collections = "collections"

primitive Empty
primitive Horizontal
primitive Vertical
primitive Bend
primitive Crossed

type Field is (Empty | Horizontal | Vertical | Bend | Crossed)

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
  let _data: collections.Map[String, Field] = _data.create()
  var _nearest_distance: I32 = 2147483647

  be access(fn: {(SharedMap ref)} val) =>
    fn(this)

  be write(key: String, value: Field) =>
    write_now(key, value)

  be read(key: String, fn: {(Field)} val) =>
    fn(read_now(key))

  fun ref write_now(key: String, value: Field) =>
    _data(key) = value

  fun ref read_now(key: String): Field =>
    try _data(key)? else Empty end

  fun ref update_nearest(x: I32) =>
    _nearest_distance = _nearest_distance.min(x)

  fun get_nearest(): I32 =>
    _nearest_distance

actor WirePuller
  let _map: SharedMap
  let _out: OutStream
  let _id: String
  let _position: Point
  new create(map: SharedMap, out: OutStream, id: String) =>
    _map = map
    _out = out
    _id = id
    _position = Point(0, 0)

  fun ref move(dir: Direction) =>
    let map = _map
    let out = _out

    _position.move(dir)
    let orientation = match dir
    | Up => Vertical
    | Down => Vertical
    | Right => Horizontal
    | Left => Horizontal
    end

    let key = _position.to_string()
    let distance = _position.manhattan()
    map.access({(map: SharedMap ref)(out, key) =>
      match map.read_now(key)
      | Empty =>
        map.write_now(key, orientation)
      | Horizontal =>
        if orientation is Vertical then
          // out.print(_id + " crossed at " + key)
          if distance < map.get_nearest() then
            out.print(_id + " found new shortest " + distance.string() + " at " + key)
            map.update_nearest(distance)
          end
          map.write_now(key, Crossed)
        end
      | Vertical =>
        if orientation is Horizontal then
          // out.print(_id + " crossed at " + key)
          if distance < map.get_nearest() then
            out.print(_id + " found new shortest " + distance.string() + " at " + key)
            map.update_nearest(distance)
          end
          map.write_now(key, Crossed)
        end
      end
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
      map.write(_position.to_string(), Bend)
      while count < steps do
        move(dir)
        count = count + 1
      end
      // out.print("Puller " + _id + " pulled to " + _position.to_string())
    end

actor Main
  new create(env: Env) =>
    let fieldmap = SharedMap
    let out = env.out

    try
      var puller_id: I32 = 0
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        for line in file.lines() do
          let puller = WirePuller(fieldmap, out, "P" + puller_id.string())
          for command in line.split(",").values() do
            puller.move_many(command)
          end
          puller_id = puller_id + 1
        end
      end
    end
