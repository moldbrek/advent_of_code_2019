use "files"
use collections = "collections"

actor Main
  new create(env: Env) =>
    let orbitMap: collections.Map[String, String] = orbitMap.create()
    let out = env.out
    var a: String = ""
    var b: String = ""

    try
      let file_name = env.args(1)?
      a = try env.args(2)? else "YOU" end
      b = try env.args(3)? else "SAN" end
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        for line in file.lines() do
          let record = line.split(")")
          orbitMap(record(1)?) = record(0)?
        end
      end
    end

    var totalOrbits: USize = 0
    for orbiter in orbitMap.keys() do
      totalOrbits = totalOrbits + getOrbitArray(orbiter, orbitMap).size()
    end
    out.print(totalOrbits.string())

    let a_orbits = getOrbitArray(a, orbitMap)
    let b_orbits = getOrbitArray(b, orbitMap)
    while a_orbits.size() > 0 do
      if not (try a_orbits.pop()? is b_orbits.pop()? else true end) then
        break
      end
    end
    out.print((a_orbits.size() + b_orbits.size()).string())

  fun getOrbitArray(key: String, map: collections.Map[String, String]): Array[String] =>
    let orbitArray: Array[String] = []
    var orbits: (String | None) = try map(key)? else None end
    while not (orbits is None) do
      orbitArray.push(try orbits as String else "" end)
      orbits = try map(orbits as String)? else None end
    end
    orbitArray
