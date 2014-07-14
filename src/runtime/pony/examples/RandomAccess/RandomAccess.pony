actor Main
  val env:Env{val}
  var streamer_count:U64
  var updater_count:U64
  var updates:U64
  var to:Array[Updater]{val}
  var start:Time

  new create(env':Env{val}) ->
    env = env

    var arg = env.args()
    var logtable = 20
    var iterate = 10000
    var chunk = 1024

    streamer_count = 4
    updater_count = 8

    try
      logtable = arg("logtable").to_U64()
      iterate = arg("iterate").to_U64()
      chunk = arg("chunk").to_U64()

      streamer_count = arg("streamers").to_U64()
      updater_count = arg("updaters").to_U64().next_pow2()
    end

    var size = (1 << logtable) / updater_count
    updates = chunk * iterate * streamer_count

    // build immutable array of Updater actors
    to = recover(
      var updaters = Array[Updater]
      updaters.reserve(updater_count)

      for i in Range(0, updater_count - 1) do
        updaters.append(Updater(i, size))
      end

      updaters
      )

    // kick off a collection of Streamer actors
    start = Time.now()

    for i in Range(1, streamer_count) do
      Streamer(this, to, size, chunk, chunk * iterate * i)(iterate)
    end

  be streamer_done() ->
    if (streamer_count = streamer_count - 1) == 1 then
      for u in to do
        u.done(this)
      end
    end

  be updater_done() ->
    if (updater_count = updater_count - 1) == 1 then
      env.print("Time: " + (Time.now() - start).string())
    end

actor Streamer
  val main:Main
  val updaters:Array[Updater]{val}
  val shift:U64
  val mask:U64
  val chunk:U64
  val rand:PolyRand{var}

  new create(main':Main, updaters':Array[Updater]{val}, size:U64, chunk':U64, seed:U64) ->
    main = main'
    updaters = updaters'
    shift = size.log2()
    mask = updaters.size() - 1
    chunk = chunk'
    rand = PolyRand(seed)

  be apply(iterate:U64) ->
    var list = recover(Array[Array[U64]{iso}].reserve(updaters.size()))

    for i in Range(1, updaters.size()) do
      list.append(recover(Array[U64].reserve(chunk)))
    end

    for i in Range(1, chunk) do
      var datum = rand()
      var updater = (datum >> shift) and mask
      list(updater).append(datum)
    end

    var vlist:Array[Array[U64]{iso}]{val} = list

    for i in vlist.indices() do
      var data = vlist(i)

      if data.size() > 0 then
        updaters(i)(data)
      end
    end

    if iterate > 0 then
      apply(iterate - 1)
    else
      main.streamer_done()
    end

actor Updater
  var table:Array[U64]{var}

  new create(index:U64, size:U64) ->
    table = Array[U64].init(size, 0)

    var offset = index * size;
    for i in Range(0, size - 1) do
      table(i) = i + offset
    end

  be apply(data:Array[U64]{val}) ->
    for datum in data do
      var i = datum and (size - 1)
      table(i) = table(i) xor datum
    end

  be done(main:Main) -> main.updater_done()

class PolyRand
  val poly:U64
  val period:U64
  var last:U64

  new create(seed:U64) ->
    poly = 7
    period = 1317624576693539401
    last = 1
    _seed(seed)

  fun{var} apply():U64 ->
    last = (last << 1) xor if last and (1 << 63) != 0 then poly else 0 end

  fun{var} _seed(seed:U64) ->
    var n = seed % period

    if n == 0 then
      last = U64(1)
    else
      var m2 = Array[U64].init(64, 0)
      last = U64(1)

      for i in Range[U64](0, 63) do
        m2(i) = last
        apply()
        apply()
      end

      var i = 64 - n.clz()
      last = U64(2)

      while i > 0 do
        var temp = 0

        for j in Range[U64](0, 63) do
          if ((last >> j) and 1) != 0 then
            temp = temp xor m2(j)
          end
        end

        last = temp
        i = i - 1

        if ((n >> i) and 1) != 0 then
          apply()
        end
      end
    end
