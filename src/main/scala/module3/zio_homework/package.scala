package module3

import zio.ZIO
import scala.language.postfixOps

import java.io.IOException
import java.util.concurrent.TimeUnit
import java.nio.file.Paths

import scala.language.{existentials, implicitConversions, postfixOps}

import zio.console.{Console, getStrLn, putStr, putStrLn}
import zio.random.{Random, nextInt, nextIntBetween}
import zio.duration.{Duration, durationInt}
import zio.clock.Clock
import zio.clock
import zio.{Has, Task, ULayer, ZIO, ZLayer, URIO}

import pureconfig.ConfigSource
import pureconfig.generic.auto._


package object zio_homework {
    /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */

  lazy val guessProgram: ZIO[Console with Random, Throwable, Unit] =
    for {
      r <- nextIntBetween(1, 4)

      _ <- putStr("Введите число от 1 до 3: ")
      s <- getStrLn
      n = s.toInt

      _ <- if (n == r) {
        putStrLn("Вы угадали!")
      } else {
        putStrLn("Вы НЕ угадали.")
        putStr("Правильный ответ: ")
        putStrLn(r.toString)
      }
    } yield ()


  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */
  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    for {
      guess <- body
      answer <- if (!condition(guess)) { doWhile(body)(condition) } else {ZIO.succeed(guess)}
    } yield answer

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */
  case class Config(endpoint: String, port: Int)
  def loadConfigOrDefault(filename: String, default: Config): Task[Config] =
    Task.effect(ConfigSource.file(Paths.get(filename)).loadOrThrow[Config]).orElse(ZIO.succeed(default))

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   *  4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   *  Используйте сервис zio Random
   */
//  val eff = ???
  lazy val eff: ZIO[Console with clock.Clock with Random, Throwable, Int] =
    for {
      _ <- ZIO.sleep(Duration.fromMillis(1000))
      n <- nextIntBetween(0, 11)
      s = n.toString
      _ <- putStrLn(s)
    } yield n

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  val effects: List[ZIO[Console with Clock with Random, Throwable, Int]] = List.range(1, 11).map(_ => eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */
  def app(effects: List[ZIO[Console with Clock with Random, Throwable, Int]]): ZIO[Console with Clock with Random, Throwable, Int] =
    for {
      numbers <- ZIO.collectAll(effects)
      n = numbers.sum
      s = n.toString
      _ <- putStrLn(s)
    } yield n

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */
  def appSpeedUp(effects: List[ZIO[Console with Clock with Random, Throwable, Int]]): ZIO[Console with Clock with Random, Throwable, Int] =
    for {
      numbers <- ZIO.collectAllPar(effects)
      n = numbers.sum
      s = n.toString
      _ <- putStrLn(s)
    } yield n

  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
  object moduleTiming {
    type Timing = Has[Timing.Service]
    object Timing {
      trait Service {
        def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Any, A]
      }

      val live: ZLayer[Has[Console.Service] with Has[Clock.Service], Nothing, Has[Service]] = ZLayer.fromServices[Console.Service, Clock.Service, Service] {
        (console: Console.Service, clock: Clock.Service) =>
          new Service {
            override def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Any, A] = for {
              start <- clock.currentTime(TimeUnit.SECONDS)
              r <- zio
              finish <- clock.currentTime(TimeUnit.SECONDS)
              _ <- console.putStrLn(s"Running time ${finish - start}")
            } yield r
          }
      }
    }

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Timing, Any, A] =
      for {
        timing <- ZIO.access[Timing](_.get)
        r <- timing.printEffectRunningTime(zio)
      } yield r

  }

  def printEffectRunningTimeWithService[R, E, A](zio: ZIO[R, E, A]): ZIO[R with moduleTiming.Timing, Any, A] = for {
    r <- moduleTiming.printEffectRunningTime(zio)
  } yield r

}
