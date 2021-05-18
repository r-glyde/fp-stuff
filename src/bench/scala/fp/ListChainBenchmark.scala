package fp

import fp.data.Chain
import org.scalameter.api._

class ListChainBenchmark extends Bench.LocalTime {

  val lists = Gen
    .range("list-size")(300000, 1500000, 300000)
    .map(size => List.fill(size)(10))

  val chains = Gen
    .range("chain-size")(300000, 1500000, 300000)
    .map(size => Chain.fill(size)(10))

  performance of "List" in {
    measure method "prepend" in {
      using(lists) in { xs =>
        0 +: xs
      }
    }

    measure method "append" in {
      using(lists) in { xs =>
        xs :+ 0
      }
    }

    measure method "concat" in {
      using(lists) in { xs =>
        xs ++ xs
      }
    }

    measure method "map" in {
      using(lists) in { xs =>
        xs.map(_ + 1)
      }
    }
  }

  performance of "Chain" in {
    measure method "prepend" in {
      using(chains) in { xs =>
        0 +: xs
      }
    }

    measure method "append" in {
      using(chains) in { xs =>
        xs :+ 0
      }
    }

    measure method "concat" in {
      using(chains) in { xs =>
        xs ++ xs
      }
    }

    measure method "map" in {
      using(chains) in { xs =>
        xs.map(_ + 1)
      }
    }
  }

}
