package chess

import org.scalatest.FunSuite
import com.github.blemale.scaffeine.{ LoadingCache, Scaffeine }
import java.util.concurrent.atomic.AtomicInteger


case class CacheValue(v: Double)


class CacheTest extends FunSuite {

	val maxElements = 100

	def loadKey(id: Long): CacheValue = new CacheValue(id * 2.0)

	val count = new AtomicInteger(0)

	val cache: LoadingCache[Long, CacheValue] = Scaffeine()
        .recordStats()
        .maximumSize(maxElements)
        .build((key: Long) => {
        	count.incrementAndGet
        	loadKey(key)
        })

	test("loading") {
		count.set(0)
		cache.underlying.invalidateAll

		assert(cache.get(1) === new CacheValue(2.0))
	}

	test("caching") {
		count.set(0)
		cache.underlying.invalidateAll

		cache.get(1)
		cache.get(1)
		cache.get(1)

		assert(count.get === 1)
	}

	test("thread safe") {
		def check: Unit = {
			val rand = scala.util.Random
			for (i <- 0 to 1000000) {
				val k = rand.nextInt(100)
				assert(cache.get(k) === new CacheValue(k * 2.0))
			}
		}

		val tasks = Vector(check _, check _, check _, check _,
							check _, check _, check _, check _)
		
		val results = tasks.par.map(_()).seq
	}

	test("max number of elements") {
		val rand = scala.util.Random

		// When retrieving less than the nb of elements in the cache,
		// the number of loading is limited
		count.set(0)
		cache.underlying.invalidateAll

		for (i <- 0 to 1000000) {
			val k = rand.nextInt(maxElements)
			cache.get(k)
		}

		assert(count.get <= maxElements)

		// When retrieving more than the nb of elements in the cache,
		// the number of loading tends to be infinite
		count.set(0)
		cache.underlying.invalidateAll

		for (i <- 0 to 1000000) {
			val k = rand.nextInt(maxElements * 2)
			cache.get(k)
		}

		assert(count.get > maxElements * 2)
	}

}
