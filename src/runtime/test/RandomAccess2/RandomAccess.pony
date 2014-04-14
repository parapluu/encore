actor Main
  val env:Env{val}
  var actor_count:U64
  var updates:U64
  var actors:Array[Updater]{val}
  var start:Time

  new create(env':Env{val}) =>
    env = env;

    var arg = env.args();
    var logtable = 20;
    var iterate = 10000;
    var chunk = 1024;

    actor_count = 4;

    try
      logtable = arg("logtable").to_U64();
      iterate = arg("iterate").to_U64();
      chunk = arg("chunk").to_U64();
      actor_count = arg("actors").to_U64();
    end

    var size = (1 << logtable) / actor_count;
    updates = chunk * iterate * actor_count;

    actors = recover(
      var a = Array[Updater];
      a.reserve(actor_count);

      for i in Range(0, actor_count - 1) do
        a.append(Updater(this, i, size, chunk, chunk * iterate * i))
      end;
      a
      );

    for a in actors do
      a.neighbours(actors)
    end;

    start = Time.now();

    for a in actors do
      a(iterate)
    end;

  be done() =>
    actor_count = actor_count - 1;
    if actor_count == 0 then
      for a in actors do
        a.done()
      end
    end

  be confirm() =>
    actor_count = actor_count + 1;
    if actor_count == actors.size() then
      env.print("Time: " + (Time.now() - start).to_String())
    end

actor Updater
  val main:Main
  val index:U64
  val shift:U64
  val mask:U64
  val chunk:U64
  val rand:PolyRand{var}
  val reuse:List[Array[U64]{iso}]{var}
  var others:(Array[Updater]{val}|None)
  var table:Array[U64]{var}

  new create(main':Main, index':U64, size:U64, chunk':U64, seed:U64) =>
    main = main';
    index = index';
    shift = size.log2();
    mask = updaters.size() - 1;
    chunk = chunk';
    rand = PolyRand(seed);
    reuse = List[Array[U64]{iso}];
    others = None;
    table = Array[U64].init(size, 0);

    var offset = index * size;
    for i in Range(0, size - 1) do
      table(i) = i + offset
    end

  be neighbours(others':Array[Updater]{val}) =>
    others = others'

  be apply(iterate:U64) =>
    var list = recover(Array[Array[U64]{iso}].reserve(updaters.size()));

    for i in Range(1, updaters.size()) do
      list.append(
        try
          reuse.pop()
        else
          recover(Array[U64].reserve(chunk))
        end
        )
    end

    for i in Range(1, chunk) do
      var datum = rand();
      var updater = (datum >> shift) and mask;
      if updater == index then
        table(i) = table(i) xor datum
      else
        list(updater).append(datum)
      end
    end;

    var vlist:Array[Array[U64]{iso}]{val} = list;

    for i in vlist.indices() do
      var data = vlist(i);

      if data.size() > 0 then
        updaters(i)(data)
      end
    end;

    if iterate > 0 then
      apply(iterate - 1)
    else
      main.done()
    end

  be receive(data:Array[U64]{iso}) =>
    for datum in data do
      var i = datum and (size - 1);
      table(i) = table(i) xor datum
    end;
    reuse.push(data)

  be done() =>
    main.confirm()

class PolyRand
  val poly:U64 = 7
  val period:U64 = 1317624576693539401
  var last:U64 = 1

  new create(seed:U64) =>
    _seed(seed)

  fun{var} apply():U64 =>
    last = (last << 1) xor (if (last and (1 << 63)) != 0 then poly else 0 end)

  fun{var} _seed(seed:U64) =>
    var n = seed % period;
    if n == 0 then
      last = U64(1)
    else
      var m2 = Array[U64].init(64, 0);
      last = U64(1);

      for i in Range[U64](0, 63) do
        m2(i) = last;
        apply();
        apply();
      end;

      var i = 64 - n.clz();
      last = U64(2);

      while i > 0 do
        var temp = 0;

        for j in Range[U64](0, 63) do
          if ((last >> j) and 1) != 0 then
            temp = temp xor m2(j)
          end
        end;

        last = temp;
        i = i - 1;

        if ((n >> i) and 1) != 0 then
          apply()
        end
      end
    end
