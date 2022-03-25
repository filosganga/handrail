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
  val source = "Hello {{.}}!"

  val handrailHelpersRegistry = HelpersRegistry.default
  val data = ast.Expression.Value.String("foo")

  val handrailTemplate = Handrail.parse(source).getOrElse(throw new RuntimeException)

  val handlebars = new Handlebars();
  val handlebarsTemplate = handlebars.compileInline(source);
}

class HandrailBenchmark {

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def handrailEval(): Unit = {
    Handrail
      .eval(
        HandrailBenchmark.handrailTemplate,
        HandrailBenchmark.data,
        HandrailBenchmark.handrailHelpersRegistry
      )
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def handlebarsEval(): Unit = {
    HandrailBenchmark
      .handlebarsTemplate("foo")
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def handrailParse(): Unit = {
    Handrail.parse(HandrailBenchmark.source).getOrElse(throw new RuntimeException)
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.Throughput))
  @OutputTimeUnit(TimeUnit.SECONDS)
  def handlebarsParse(): Unit = {
    HandrailBenchmark.handlebars.compileInline(HandrailBenchmark.source);
  }
}
