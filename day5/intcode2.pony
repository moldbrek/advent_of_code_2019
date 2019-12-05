use "files"

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

type Mode is (Position | Immediate)

class Program
  let _input: I32
  let _out :OutStream
  var _index : USize
  var _program : Array[I32]

  new create(input: I32, out: OutStream, program: Array[I32]) =>
    _input = input
    _out = out
    _index = 0
    _program = program

  fun read_value(index: USize, mode: Mode): I32 =>
    match mode
    | Position =>
      try
        _program(_program(index)?.usize())?
      else
        _out.print("Invalid position for parameter: " + index.string())
        0
      end
    | Immediate => try _program(index)? else 0 end
    end

  fun ref calculate(op: CalcOperation, m1: Mode, m2: Mode, m3: Mode) =>
    let v1 = read_value(_index + 1, m1)
    let v2 = read_value(_index + 2, m2)
    let result = match op
    | Add => v1 + v2
    | Multiply => v1 * v2
    | LessThan => if v1 < v2 then 1 else 0 end
    | Equals => if v1 == v2 then 1 else 0 end
    end
    try
      _program(_program(_index + 3)?.usize())? = result
    else
      _out.print("Invalid position for result: " + (_index + 3).string())
    end
    _index = _index + 4

  fun ref write_input() =>
    try
      _program(_program(_index + 1)?.usize())? = _input
    else
      _out.print("Invalid position for input: " + (_index + 1).string())
    end
    _index = _index + 2

  fun ref read_output(m: Mode) =>
    let value = read_value(_index + 1, m)
    _out.print("Output: " + value.string())
    _index = _index + 2

  fun ref jump(op: JumpOperation, m1: Mode, m2: Mode) =>
    let do_jump = match op
    | JumpIf => read_value(_index + 1, m1) != 0
    | JumpIfNot => read_value(_index + 1, m1) == 0
    end
    if do_jump then
      _index = read_value(_index + 2, m2).usize()
    else
      _index = _index + 3
    end

  fun ref execute() =>
    while _index < _program.size() do
      let opcode = try _program(_index)? else 0 end
      let m1 = if ((opcode / 100) % 10) == 1 then Immediate else Position end
      let m2 = if ((opcode / 1000) % 10) == 1 then Immediate else Position end
      let m3 = if ((opcode / 10000) % 10) == 1 then Immediate else Position end

      match opcode % 100
      | 1 => calculate(Add, m1, m2, m3)
      | 2 => calculate(Multiply, m1, m2, m3)
      | 3 => write_input()
      | 4 => read_output(m1)
      | 5 => jump(JumpIf, m1, m2)
      | 6 => jump(JumpIfNot, m1, m2)
      | 7 => calculate(LessThan, m1, m2, m3)
      | 8 => calculate(Equals, m1, m2, m3)
      | 99 =>
        _out.print("Program ended normally")
        break
      else
        _out.print("Program ended with invalid opcode")
        break
      end
    end

actor Main
  new create(env: Env) =>
    try
      let file_name = env.args(1)?
      let input = env.args(2)?.i32()?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        let text = file.lines().next()?
        let codes : Array[I32] = []
        for code in text.split(",").values() do
          codes.push(code.i32()?)
        end
        Program(input, env.out, codes).execute()
      end
    end
