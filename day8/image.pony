use "files"
use "itertools"
use "promises"

class val LayerSummary
  let _layer_id : I32
  let _summary : Array[I32] val

  new create(layer_id: I32, summary: Array[I32] val) =>
    _layer_id = layer_id
    _summary = recover summary end

  fun get_layer_id() : I32 =>
    _layer_id

  fun get_count(key: I32) : I32 =>
    try _summary(key.usize())? else 0 end

actor LayerAggregate
  let _layer_id : I32
  let _out: OutStream
  let _summary : Array[I32]

  new create(layer_id: I32, out: OutStream) =>
    _layer_id = layer_id
    _out = out
    _summary = [0; 0; 0]

  be analyze_layer(data: String val) =>
    for c in data.values() do
      var i: USize = (c - 48).usize()
      try _summary(i)? = _summary(i)? + 1 end
    end

  be get_summary(p: Promise[LayerSummary]) =>
    let summary_trn : Array[I32] trn = recover Array[I32] end
    for v in _summary.values() do
      summary_trn.push(v)
    end
    let summary_val : Array[I32] val = consume summary_trn
    p(recover LayerSummary(_layer_id, summary_val) end)

actor Main
  var image_data: String = ""
  var rows: I32 = 0
  var cols: I32 = 0
  let _env: Env

  new create(env: Env) =>
    _env = env

    try
      let file_name = env.args(1)?
      rows = env.args(2)?.i32()?
      cols = env.args(3)?.i32()?
      let path = FilePath(env.root as AmbientAuth, file_name)?
      match OpenFile(path)
      | let file: File =>
        image_data = file.lines().next()?
      end
    end

    var layer_size = rows * cols
    var num_layers = image_data.size().i32() / layer_size
    var layer_indexes: Array[I32] = []
    var i: I32 = 0
    var layers: Array[String] = []
    while i < num_layers do
      layer_indexes.push(i)
      layers.push(image_data.substring(
        (i * layer_size).isize(), ((i + 1) * layer_size).isize()))
      i = i + 1
    end

    var message: String ref = recover String(layer_size.usize()) end
    i = 0
    while i < layer_size do
      for layer in layers.values() do
        var c = try layer.at_offset(i.isize())? else '2' end
        if c != '2' then
          match c
          | '0' => message.push(' ')
          | '1' => message.push('#')
          end
          break
        end
      end
      i = i + 1
    end
    i = 0
    while i < layer_size do
      env.out.print(message.substring(i.isize(), (i + cols).isize()))
      i = i + cols
    end

    let create_summary_promise =
    {(id: I32): Promise[LayerSummary] =>
      let aggregate = LayerAggregate(id, env.out)
      aggregate.analyze_layer(
        recover image_data.substring(
          (id * layer_size).isize(),
          ((id + 1) * layer_size).isize()) end)
      let p = Promise[LayerSummary]
      aggregate.get_summary(p)
      p
    } iso

    Promises[LayerSummary].join(
      Iter[I32](layer_indexes.values())
        .map[Promise[LayerSummary]](consume create_summary_promise))
      .next[None](recover this~receive_collection() end)

  be receive_collection(coll: Array[LayerSummary] val) =>
    _env.out.print("Received layer summaries:")
    var fewest_zeros = rows * cols
    var answer: I32 = 0
    for summary in coll.values() do
      if summary.get_count(0) < fewest_zeros then
        fewest_zeros = summary.get_count(0)
        answer = summary.get_count(1) * summary.get_count(2)
      end
    end
    _env.out.print("Answer is: " + answer.string())
