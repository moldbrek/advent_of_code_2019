use "files"

primitive Add
primitive Multiply
primitive End

type Operation is (Add | Multiply | End)

actor CodeChecker
  let codes : Array[U64] val

  new create(codes': Array[U64] val) =>
    codes = codes'

  be check_pair(noun: U64, verb: U64, comp: U64, env: Env) =>
    var step : U64 = 0
    var val1 : U64 = 0
    var val2 : U64 = 0
    var operation : Operation = Add
    let codes_clone = codes.clone()
    try
      codes_clone(1)? = noun
      codes_clone(2)? = verb
      for value in codes_clone.values() do
        match step
        | 0 =>
          operation = match value
          | 1 => Add
          | 2 => Multiply
          else
            End
            break
          end
          if operation is End then break end
        | 1 => val1 = codes_clone(value.usize())?
        | 2 => val2 = codes_clone(value.usize())?
        | 3 =>
          let result : U64 = match operation
          | Add => val1 + val2
          | Multiply => val1 * val2
          else
            0
          end
          codes_clone(value.usize())? = result
        end
        step = (step + 1) % 4
      end
      if codes_clone(0)? == comp then
        env.out.print("Answerx: " + codes_clone(0)?.string())
        env.out.print(((100 * noun) + verb).string())
      end
    end

actor Main

  new create(env: Env) =>
    try
      let file_name = env.args(1)?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        let text : String = file.read_string(2048)
        let codes : Array[U64] trn = recover Array[U64] end
        for code in text.split(",").values() do
          let codeclone = code.clone()
          codeclone.strip()
          codes.push(codeclone.u64()?)
        end
        let codesval : Array[U64] val = consume codes
        var i : U64 = 0
        var j : U64 = 0
        while i < 100 do
          while j < 100 do
            let checker = CodeChecker(codesval)
            checker.check_pair(i, j, 19690720, env)
            j = j + 1
          end
          i = i + 1
          j = 0
        end
      end
    end
