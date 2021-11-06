package handrail
package benchmark

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit
import cats.effect.unsafe.implicits.global
import cats._
import cats.syntax.all._
import cats.effect._
import cats.effect.implicits._
import org.typelevel.log4cats.slf4j.Slf4jLogger
import com.github.jknack.handlebars._

object HandrailBenchmark {
  val logger = Slf4jLogger.getLogger[IO]
  val registry = HelpersRegistry.default[IO]
  val output = Output.devNull[IO]
  val data = ast.Expression.Value.String("foo")
  val expression = ast.Expression.Function(
    "render",
    List(
      ast.Expression.Function(
        "lookup",
        List(
          data,
          ast.Expression.Value.String(".")
        ),
        Map.empty
      )
    ),
    Map.empty
  )

  val handlebars = new Handlebars();
  val template = handlebars.compileInline("{{.}}!");
}

class HandrailBenchmark {

  // @Benchmark
  // @BenchmarkMode(Array(Mode.Throughput))
  // @OutputTimeUnit(TimeUnit.SECONDS)
  // def handrail(): Unit = {
  //   Handrail
  //     .eval[IO](
  //       HandrailBenchmark.expression,
  //       HandrailBenchmark.data,
  //       HandrailBenchmark.output,
  //       HandrailBenchmark.registry,
  //       HandrailBenchmark.logger
  //     )
  //     .unsafeRunSync()
  // }

  // @Benchmark
  // @BenchmarkMode(Array(Mode.Throughput))
  // @OutputTimeUnit(TimeUnit.SECONDS)
  // def handlebars(): Unit = {
  //   HandrailBenchmark
  //     .template("foo")
  // }
}
