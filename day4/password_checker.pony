actor SharedCount
  let _out: OutStream
  var _count : I32

  new create(out: OutStream) =>
    _out = out
    _count = 0

  be increment() =>
    _count = _count + 1
    let out = _out
    out.print("Number of ok passwords is: " + _count.string())

actor PasswordChecker
  let _okCount: SharedCount

  new create(okCount: SharedCount) =>
    _okCount = okCount

  be check(p: String) =>
    let okCount = _okCount

    if p.size() == 6 then
      var p1: U8 = 0
      var p2: U8 = 0
      var hasDouble = false
      for c in p.array().values() do
        if (p1 == c) then
          if (p2 != p1) then
            hasDouble = true
          else
            hasDouble = false
          end
        else
          if (p2 == p1) and hasDouble then
            break
          end
        end
        p2 = p1
        p1 = c
      end
      p1 = 0
      var isIncreasing = true
      for c in p.array().values() do
        if c < p1 then
          isIncreasing = false
          break
        end
        p1 = c
      end
      if isIncreasing and hasDouble then
        okCount.increment()
      end
    end

actor Main
  new create(env: Env) =>
      let out = env.out
      let okCount = SharedCount(out)

      var i: I32 = try env.args(1)?.i32()? else 111122 end
      let max: I32 = try env.args(2)?.i32()? else i end

      while i <= max do
        PasswordChecker(okCount).check(i.string())
        i = i + 1
    end
