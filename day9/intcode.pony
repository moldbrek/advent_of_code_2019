use "files"
use collections = "collections"

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
    _out.print("Reading output " + _read_value(_index + 1, m).string())
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

  fun ref get_output(): I64 =>
    try _outputs.pop()? else 0 end

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

actor Main
  var input: I64 = 0
  new create(env: Env) =>
    let codes : Array[I64] = []
    try
      let file_name = env.args(1)?
      input = env.args(2)?.i64()?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        let text = file.lines().next()?
        for code in text.split(",").values() do
          codes.push(code.i64()?)
        end
      end
    end

    let program = Program(env.out, codes)
    program.execute([input])
    env.out.print("Output " + program.get_output().string())
