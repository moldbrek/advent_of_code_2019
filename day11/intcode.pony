use "files"
use collections = "collections"

primitive Up
primitive Down
primitive Left
primitive Right

type Direction is (Up | Down | Left | Right)

class val Point is Comparable[Point]
  var _x: I32
  var _y: I32

  new create(x: I32, y: I32) =>
    _x = x
    _y = y

  fun get_x(): I32 => _x
  fun get_y(): I32 => _y

  fun string(): String =>
    _x.string() + "," + _y.string()

  fun hash(): USize =>
    string().hash()

  fun manhattan(): I32 =>
    (_x.abs() + _y.abs()).i32()

  fun move(dir: Direction): Point =>
    match dir
    | Up => recover Point(_x, _y + 1) end
    | Down => recover Point(_x, _y - 1) end
    | Right => recover Point(_x + 1, _y) end
    | Left => recover Point(_x - 1, _y) end
    end

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

class Pose
  var _position : Point
  var _orientation : Direction

  new create(position: Point, orientation: Direction) =>
    _position = position
    _orientation = orientation

  fun ref turn(dir: (Left | Right)) =>
    _orientation = match _orientation
    | Up => if dir is Right then Right else Left end
    | Down => if dir is Right then Left else Right end
    | Right => if dir is Right then Down else Up end
    | Left => if dir is Right then Up else Down end
    end

  fun ref move_forward() =>
    _position = _position.move(_orientation)

  fun get_position(): Point => _position
  fun get_orientation(): Direction => _orientation

primitive Add
primitive Multiply
primitive Input
primitive Output
primitive JumpIf
primitive JumpIfNot
primitive LessThan
primitive Equals
primitive End

type CalcOperation is (Add | Multiply | LessThan | Equals)
type JumpOperation is (JumpIf | JumpIfNot)

primitive Position
primitive Immediate
primitive Relative

type Mode is (Position | Immediate | Relative)

primitive Running
primitive AwaitingInput
primitive Halted
primitive Failed

type RunStatus is (Running | AwaitingInput | Halted | Failed)

class Program
  let _out :OutStream
  var _program : collections.Map[USize, I64]
  var _inputs: Array[I64]
  var _outputs : Array[I64]
  var _index : USize
  var _relative_base : I64

  new create(out: OutStream, program: Array[I64]) =>
    _out = out
    _program = _program.create()
    _inputs = []
    _outputs = []
    _index = 0
    _relative_base = 0

    var i: USize = 0
    while i < program.size() do
      try _program(i) = program(i)? end
      i = i + 1
    end

  fun _read_value(index: USize, mode: Mode): I64 =>
    match mode
    | Position => try _program(_program(index)?.usize())? else 0 end
    | Immediate => try _program(index)? else 0 end
    | Relative => try _program((_program(index)? + _relative_base).usize())?
      else 0 end
    end

  fun ref _write_value(index: USize, mode: Mode, value: I64) =>
    match mode
    | Position => try _program(_program(index)?.usize()) = value end
    | Immediate => _program(index) = value
    | Relative => try
        _program((_program(index)? + _relative_base).usize()) = value end
    end

  fun ref _calculate(op: CalcOperation, m1: Mode, m2: Mode, m3: Mode) =>
    let v1 = _read_value(_index + 1, m1)
    let v2 = _read_value(_index + 2, m2)
    let result = match op
    | Add => v1 + v2
    | Multiply => v1 * v2
    | LessThan => if v1 < v2 then 1 else 0 end
    | Equals => if v1 == v2 then 1 else 0 end
    end
    _write_value(_index + 3, m3, result)
    _index = _index + 4

  fun ref _write_input(m: Mode) =>
    try _write_value(_index + 1, m, _inputs.pop()?) end
    _index = _index + 2

  fun ref _read_output(m: Mode) =>
    _outputs.push(_read_value(_index + 1, m))
    _index = _index + 2

  fun ref _jump(op: JumpOperation, m1: Mode, m2: Mode) =>
    let do_jump = match op
    | JumpIf => _read_value(_index + 1, m1) != 0
    | JumpIfNot => _read_value(_index + 1, m1) == 0
    end
    if do_jump then
      _index = _read_value(_index + 2, m2).usize()
    else
      _index = _index + 3
    end

  fun ref _adjust_relative_base(m: Mode) =>
    _relative_base = _relative_base + _read_value(_index + 1, m)
    _index = _index + 2

  fun ref get_outputs(): Array[I64] =>
    let outputs = _outputs.clone()
    _outputs.clear()
    outputs

  fun get_mode(mode_code: I64): Mode =>
    match mode_code
    | 0 => Position
    | 1 => Immediate
    | 2 => Relative
    else
      Position
    end

  fun ref execute(inputs: Array[I64]): RunStatus =>
    _inputs.append(inputs)
    var status: RunStatus = Running
    while _index < _program.size() do
      let opcode = try _program(_index)? else 0 end
      let m1 = get_mode((opcode / 100) % 10)
      let m2 = get_mode((opcode / 1000) % 10)
      let m3 = get_mode((opcode / 10000) % 10)

      match opcode % 100
      | 1 => _calculate(Add, m1, m2, m3)
      | 2 => _calculate(Multiply, m1, m2, m3)
      | 3 =>
        if _inputs.size() > 0 then
          _write_input(m1)
        else
          status = AwaitingInput
          break
        end
      | 4 => _read_output(m1)
      | 5 => _jump(JumpIf, m1, m2)
      | 6 => _jump(JumpIfNot, m1, m2)
      | 7 => _calculate(LessThan, m1, m2, m3)
      | 8 => _calculate(Equals, m1, m2, m3)
      | 9 => _adjust_relative_base(m1)
      | 99 =>
        status = Halted
        break
      else
        status = Failed
        break
      end
    end
    status

actor PaintingRobot
  let _out: OutStream
  let _panel_map : collections.Map[Point, I64]
  let _pose : Pose

  new create(out: OutStream, input: I64) =>
    _out = out
    _panel_map = _panel_map.create()
    _pose = Pose(recover Point(0, 0) end, Up)
    _panel_map(_pose.get_position()) = input

  fun _get_color_code() : I64 =>
    try _panel_map(_pose.get_position())? else 0 end

  be execute(codes: Array[I64] val) =>
    var run_status: RunStatus = AwaitingInput
    let controller = Program(_out, codes.clone())
    while run_status is AwaitingInput do
      run_status = controller.execute([_get_color_code()])
      let output = controller.get_outputs()
      try
        _panel_map(_pose.get_position()) = output(0)?
        _out.print("Painting " + if output(0)? == 0 then "black" else "white" end +
                  " at position " + _pose.get_position().string())
        _pose.turn(if output(1)? == 0 then Left else Right end)
        _pose.move_forward()
      end
    end
    _out.print("Painted " + _panel_map.size().string() + " panels")
    var message: String ref = recover String(40*6) end
    var i: ISize = 0
    while i < (40 * 6) do
      message.push(' ')
      i = i + 1
    end
    for (position, color) in _panel_map.pairs() do
      try
        message.update_offset(
          (position.get_x() - (40 * position.get_y())).isize(),
          if color == 0 then ' ' else '#' end)?
      end
    end
    i = 0
    while i < (40 * 6) do
      _out.print(message.substring(i, i + 40))
      i = i + 40
    end

actor Main
  new create(env: Env) =>
    var input: I64 = 0
    let codes : Array[I64] trn = recover Array[I64] end
    try input = env.args(2)?.i64()? else 0 end
    try
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        let text = file.lines().next()?
        for code in text.split(",").values() do
          codes.push(code.i64()?)
        end
      end
    end

    let codesval : Array[I64] val = consume codes
    PaintingRobot(env.out, input).execute(codesval)
