(ns visi.clcore
  (:gen-class))

(import
  '(visi Frog)
  '(java.util Date)
  '(java.util.concurrent ConcurrentHashMap))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
(let [frog (Frog.)]
(println "Plus 1 " (.foo frog 1))
  (println "Pow " (Math/pow 2 3))
  (println "Map is " (ConcurrentHashMap.))
  (println "It's " (Date.))
  (println "Hello, World!33"))
  (println (map + [1 2 3] [4 5 6]))

  44)


(defn f1 [f] (proxy [scala.Function1] [] (apply [p] (f p))))
(defn f2 [f] (proxy [scala.Function2] [] (apply [p] (f p))))
(defn f3 [f] (proxy [scala.Function3] [] (apply [p] (f p))))
(defn f4 [f] (proxy [scala.Function4] [] (apply [p] (f p))))
(defn f5 [f] (proxy [scala.Function5] [] (apply [p] (f p))))
(defn f6 [f] (proxy [scala.Function6] [] (apply [p] (f p))))
(defn f7 [f] (proxy [scala.Function7] [] (apply [p] (f p))))


(defmulti cvt class)
(defmethod cvt net.liftweb.common.EmptyBox [x] [])
(defmethod cvt net.liftweb.common.Full [x] [(.openTheBox x)])
(defmethod cvt scala.Product1 [x] [(._1 x)])
(defmethod cvt scala.Product2 [x] [(._1 x) (._2 x)])
(defmethod cvt scala.Product3 [x] [(._1 x) (._2 x) (._3 x)])
(defmethod cvt scala.collection.Seq [x] (let [ret (transient [])
                                              tfn (f1 (fn [x] (conj! ret x)))]
                                          (.foreach x tfn)
                                          (persistent! ret)))
(defmethod cvt scala.collection.Map [x] (let [ret (transient {})
                                              tfn (f1 (fn [x] (conj! ret [(._1 x) (._2 x)])))]
                                          (.foreach x tfn)
                                          (persistent! ret)))


(defrecord LetExp [loc ^String id ^String name ^Boolean generic type exp])
(defrecord InnerLet [loc type exp1 exp2])
(defrecord SinkExp [loc ^String id ^String name type exp])
(defrecord SourceExp [loc ^String id ^String name type])
(defrecord FuncExp [loc ^String id ^String name type exp])
(defrecord Apply [loc ^String id type exp1 exp2])
(defrecord Var [loc ^String id ^String name type])
(defrecord BuiltIn [loc ^String id ^String name type func])
(defrecord ValueConst [loc value type])
(defrecord Group [map])

(defrecord Depends [^String whatId stack what dependents predicates])

(defmulti build-graph class)
()
(defn build-graph [stuff scope stack theExp]
  (cond
    (instance? Group theExp) ()
    ))

;
;                                               ))
;private def buildGraph(stuff: LetMap, scope: LetScope, stack: List[Expression], theExp: Expression): Box[Boolean] = {
;                                                                                                                      theExp match {
;                                                                                                                                     case LetExp(loc, id, name, generic, tpe, exp) =>
;                                                                                                                                     val newMap = scope + (name -> theExp)
;                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                     buildGraph(stuff, newMap, theExp +: stack, exp)
;
;                                                                                                                                     case InnerLet(loc, tpe, exp1, exp2) =>
;                                                                                                                                     val newScope = exp1 match {
;                                                                                                                                                                 case le@LetExp(loc, id, name, generic, tpe, exp) =>
;      scope + (name -> le)
;                                                                                                                                                                 }
;
;                                                                                                                                     reduce(Stream(buildGraph(stuff, newScope, theExp +: stack, exp1),
;                                                                                                                                                    buildGraph(stuff, newScope, theExp +: stack, exp2)))
;
;                                                                                                                                     case SinkExp(loc, id, name, tpe, exp) =>
;                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                     val newMap = scope + (name -> theExp)
;                                                                                                                                     buildGraph(stuff, newMap, theExp +: stack, exp)
;
;                                                                                                                                     case SourceExp(loc, id, name, tpe) =>
;                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                     Full(true)
;
;                                                                                                                                     case FuncExp(loc, id, name, tpe, exp) =>
;                                                                                                                                     val newScope = scope + (name -> theExp)
;                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                     buildGraph(stuff, newScope, theExp +: stack, exp)
;
;                                                                                                                                     case Apply(loc, id, tpe, exp1, exp2) =>
;                                                                                                                                     val r1 = buildGraph(stuff, scope, theExp +: stack, exp1)
;    if (r1.isDefined) buildGraph(stuff, scope, theExp +: stack, exp2)
;                                                                                                                                     else r1
;
;                                                                                                                                     case vexp@Var(loc, id, name, tpe) =>
;                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                     for {
;                                                                                                                                           target1 <- scope.get(name) ?~ ("Unable to find function " + name.name) ~> theExp
;                                                                                                                                           target <- Full(target1).asA[HasLetId]
;                                                                                                                                           } yield {
;                                                                                                                                                     crossRef(stuff, vexp, target)
;                                                                                                                                                     true
;                                                                                                                                                     }
;
;                                                                                                                                                     case BuiltIn(loc, id, name, tpe, _) =>
;                                                                                                                                                     if (!stuff.containsKey(id)) stuff.put(id, Depends(id, stack, theExp, Set.empty, Set.empty))
;                                                                                                                                                     Full(true)
;
;                                                                                                                                                     case ValueConst(loc: SourceLoc, value: Value, tpe: Type) =>
;                                                                                                                                                                                                              // Do Nothing
;                                                                                                                                                     Full(true)
;
;                                                                                                                                                     case Group(map) =>
;                                                                                                                                                     val newMap = map.foldLeft(scope) {
;                                                                                                                                                                                        case (sc, (n, e2)) => sc + (n -> e2)
;                                                                                                                                                                                        }
;
;                                                                                                                                                     map.values.foreach {
;                                                                                                                                                                          case hl: HasLetId with HasName =>
;                                                                                                                                                                          stuff.put(hl.id, Depends(hl.id, hl +: theExp +: stack, hl, Set.empty, Set.empty))
;                                                                                                                                                                          case _ =>
;        }
;
;                                                                                                                                                                          reduce(map.map(_._2).toStream.map(v => buildGraph(stuff, newMap, theExp +: stack, v)))
;
;                                                                                                                                                                          }
;                                                                                                                                                     }


(defn find-let-thingy
  [lst] (first (find #(and (:name %) (:id %))) lst))

(defn full [x] (net.liftweb.common.Full. x))
(def bempty (net.liftweb.common.Empty$/MODULE$))


(defmethod cvt visi.core.FuncName [x] (.name x))

(defmethod cvt visi.core.LetExp [x] (LetExp. (.loc x)
                                      (.id (.id x))
                                      (.name (.name x))
                                      (.generic x)
                                      (.tpe x)
                                      (.exp x)))

(defmethod cvt visi.core.InnerLet [x] (InnerLet. (.loc x)
                                        (.type x)
                                        (.exp1 x)
                                        (.exp2 x)))

(def js-stuff
  "
function Thunk(f, scope) {
  this.$_get = getFunc()

  function getFunc() {
    return function() {
      this.$_get = function() {return this.$_old;};
      var ret = f(scope);
      this.$_get = function() {return ret;}
      this.$_old = ret;
      return ret;
    };
  }

  this.$_reset = function() {
    this.$_get = getFunc()
  }

  this.$_apply = function(x) {
    return this.$_get().$_apply(x);
  }

  this.$_old = 0
}

function Value(v) {
  this.$_set = function(nv) {v = nv;};
  this.$_get = function() {return v;};

  this.$_reset = function() {};
}

function $_cloner(obj) {
    if(obj == null || typeof(obj) != 'object')
        return obj;

    var temp = {};

    for(var key in obj)
        temp[key] = obj[key];
    return temp;
}

function Func(compute, args, arity, scope, subfunc) {
  if (args.length >= arity) {
    this.$_apply = function(param) {
    var got = this.$_get();
    var ret = got.$_apply(param);
    return ret;
    };
  } else {
    this.$_apply = function(param) {
      var a1 = args;
      var a2 = [];
      for (i in a1) a2.push(a1[i]);
      a2.push(param);
      if (subfunc) return compute(scope, a2);
      return new Func(compute, a2, arity, scope, false);
    }
  }

  if (arity > args.length) {
    this.$_get = function() {return this;};
  } else {
    this.$_get = function() {
      var ret = compute(scope, args);
      this.$_get = function() {return ret;};
      return ret;
    }
  }

  this.$_reset = function() {};
}

function $_plusFunc_core(scope, args) {
  return args[0].$_get() + args[1].$_get();
}

function $_minusFunc_core(scope, args) {
  return args[0].$_get() - args[1].$_get();
}

function $_timesFunc_core(scope, args) {
  return args[0].$_get() * args[1].$_get();
}

function $_divFunc_core(scope, args) {
  return args[0].$_get() / args[1].$_get();
}

function $_ifelse_core(scope, args) {
  if (args[0].$_get()) return args[1].$_get();
  return args[2].$_get();
}

function $_swapfunc_core(scope, args) {
  var ret = new Func(function(zscope, zargs) {
   return args[0].$_apply(zargs[1]).$_apply(zargs[0]).$_get();
  }, [], 2, scope, false);

  return ret;
}

function $_concat_core(scope, args) {
  return args[0].$_get() + args[1].$_get();
}

function $_equals_core(scope, args) {
  return args[0].$_get() == args[1].$_get();
}


function Const(v) {
  this.$_get = function() {return v;}
  this.$_reset = function() {}
}


function execute(sources) {
  var sinksToRun = {};
  var lets = {};
  for (i in sources) {
    scope[i].$_set(sources[i]);
    sourceInfo[i] = true;
    var sa = sourceToSink[i];
    for (j in sa) {
      sinksToRun[sa[j]] = true;
    }

    sa = sourceToLets[i]
    for (j in sa) {
      lets[sa[j]] = true;
    }
  }

  for (i in sourceInfo) {
    if (!sourceInfo[i]) throw (\"Cannot execute because the source '\"+i+\"' has not been set\");
  }

  for (i in lets) {
    scope[i].$_reset();
  }

  var ret = {};

  for (i in sinksToRun) {
    sinks[i].$_reset();
    ret[i] = sinks[i].$_get();
  }

  return ret;
}
    ")