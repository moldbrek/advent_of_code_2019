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

primitive Running
primitive AwaitingInput
primitive Halted
primitive Failed

type RunStatus is (Running | AwaitingInput | Halted | Failed)

actor BestSignal
  let _out: OutStream
  var _best_signal: I32

  new create(out: OutStream) =>
    _out = out
    _best_signal = 0

  be update_best(signal: I32, optcodes: Array[I32] val) =>
    try
      _out.print("Signal: " + signal.string() + " with optcodes [" + optcodes(0)?.string() + "," + optcodes(1)?.string() + "," + optcodes(2)?.string() + "," + optcodes(3)?.string() + "," + optcodes(4)?.string() + "]")
    end

    if signal > _best_signal then
      _best_signal = signal
      _out.print("Found new best: " + signal.string())
    end

class Program
  let _out :OutStream
  var _program : Array[I32]
  var _inputs: Array[I32]
  var _outputs : Array[I32]
  var _index : USize

  new create(out: OutStream, program: Array[I32]) =>
    _out = out
    _program = program
    _inputs = []
    _outputs = []
    _index = 0

  fun _read_value(index: USize, mode: Mode): I32 =>
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

  fun ref _calculate(op: CalcOperation, m1: Mode, m2: Mode, m3: Mode) =>
    let v1 = _read_value(_index + 1, m1)
    let v2 = _read_value(_index + 2, m2)
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

  fun ref _write_input() =>
    try
      _program(_program(_index + 1)?.usize())? = _inputs.pop()?
    else
      _out.print("Invalid position for input: " + (_index + 1).string())
    end
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

  fun ref get_output(): I32 =>
    try _outputs.pop()? else 0 end

  fun ref execute(inputs: Array[I32]): RunStatus =>
    _inputs.append(inputs)
    var status: RunStatus = Running
    while _index < _program.size() do
      let opcode = try _program(_index)? else 0 end
      let m1 = if ((opcode / 100) % 10) == 1 then Immediate else Position end
      let m2 = if ((opcode / 1000) % 10) == 1 then Immediate else Position end
      let m3 = if ((opcode / 10000) % 10) == 1 then Immediate else Position end

      match opcode % 100
      | 1 => _calculate(Add, m1, m2, m3)
      | 2 => _calculate(Multiply, m1, m2, m3)
      | 3 =>
        if _inputs.size() > 0 then
          _write_input()
        else
          status = AwaitingInput
          break
        end
      | 4 => _read_output(m1)
      | 5 => _jump(JumpIf, m1, m2)
      | 6 => _jump(JumpIfNot, m1, m2)
      | 7 => _calculate(LessThan, m1, m2, m3)
      | 8 => _calculate(Equals, m1, m2, m3)
      | 99 =>
        status = Halted
        break
      else
        status = Failed
        break
      end
    end
    status

actor PhaseSetExecutor
  let _program: Array[I32] val
  let _phases_in: Array[I32] val
  let _shared_best : BestSignal
  var _code: I32
  let _out: OutStream

  new create(program: Array[I32] val, phases: Array[I32] val, code: I32, out: OutStream, shared_best: BestSignal) =>
    _program = program
    _phases_in = phases
    _shared_best = shared_best
    _code = code
    _out = out

  fun fac(n': I32): I32 =>
    var x: I32 = 1
    var n = n'
    while n > 1 do
      x = x * n
      n = n - 1
    end
    x

  be execute() =>
    var signal: I32 = 0
    let phases = _phases_in.clone()
    let phases_out : Array[I32] trn = recover Array[I32] end
    var controllers : Array[Program] = []
    var run = true
    var run_status: RunStatus = Running

    while phases.size() > 0 do
      var test = fac(phases.size().i32() - 1)
      var i = _code / test
      _code = _code - (test * i)
      var optcode: I32 = try phases.delete(i.usize())? else 0 end
      phases_out.push(optcode)
      let controller = Program(_out, _program.clone())
      run_status = controller.execute([signal; optcode])
      signal = controller.get_output()
      controllers.push(controller)
    end
    let phases_val : Array[I32] val = consume phases_out
    while run_status is AwaitingInput do
      for controller in controllers.values() do
        run_status = controller.execute([signal])
        signal = controller.get_output()
      end
    end
    _shared_best.update_best(signal, phases_val)
    // try
    //   _out.print("Signal: " + signal.string() + " with optcodes [" + _phases_out(0)?.string() + "," + _phases_out(1)?.string() + "," + _phases_out(2)?.string() + "," + _phases_out(3)?.string() + "," + _phases_out(4)?.string() + "]")
    // end

actor Main
  new create(env: Env) =>
    let codes : Array[I32] trn = recover Array[I32] end
    try
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        let text = file.lines().next()?
        for code in text.split(",").values() do
          codes.push(code.i32()?)
        end
      end
    end

    let codesval : Array[I32] val = consume codes
    let shared_best = BestSignal(env.out)
    var i: I32 = 0
    while i < 120 do
      PhaseSetExecutor(codesval, [5;6;7;8;9], i, env.out, shared_best).execute()
      i = i + 1
    end
