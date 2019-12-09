use "files"

primitive FuelCalc
  fun required(m: I64): I64 =>
    (m / 3) - 2

  fun requiredTotal(m: I64): I64 =>
    var total: I64 = 0
    var rest: I64 = m
    while rest > 0 do
      rest = FuelCalc.required(rest)
      total = if rest > 0 then total + rest else total end
    end
    total

actor Main
  new create(env: Env) =>
    try
      let file_name = env.args(1)?
      let task = env.args(2)?.i64()?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      var sum: I64 = 0
      match OpenFile(path)
      | let file: File =>
        for line in file.lines() do
          if task == 2 then
            sum = sum + FuelCalc.requiredTotal(line.i64()?)
          else
            if task == 1 then
              sum = sum + FuelCalc.required(line.i64()?)
            end
          end
        end
      else
        env.err.print("Error opening file '" + file_name + " '")
      end
      env.out.print("Sum: " + sum.string())
    end
