package module3

import zio.{ExitCode, Has, Task, ULayer, URIO, ZIO, ZLayer, clock, console, random}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import java.util.concurrent.TimeUnit

import zio.random

import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Random

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */

  def getNumberFromStringAndCompare(number: Int) = for {
    _ <- console.putStrLn("Введите ваше предполагаемое число ниже...")
    guessed <- console.getStrLn.map(str => str.toInt == number).resurrect
      .foldM(
        _ => console.putStrLn("Вы ввели не целочисленное значение, пожалуйста, сконцентрируйтесь и попробуйте еще раз") *> ZIO.succeed(false),
        res => ZIO.succeed(res))

  } yield (guessed)

  lazy val guessProgram = for {
    number <- random.nextIntBetween(1, 4)
    _ <- console.putStrLn("Программа задумала число от 1 до 3 попробуйте угадать.")
    guessed <- getNumberFromStringAndCompare(number).repeatWhile(!_)
    _ <- if (guessed) console.putStr("Вы угадали !!!") else console.putStr(s"К сожалению вы не угадали правильное число $number")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[A](condition: A => Boolean, effect: ZIO[_, _, A]) = effect.repeatWhile(condition)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault =
    config.load.orElse(ZIO.succeed(new config.AppConfig("MyApplication", "proto://default")))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: ZIO[random.Random with Clock, Nothing, Int] = random.nextIntBetween(0, 10).delay(1 second)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: Seq[ZIO[random.Random with Clock, Nothing, Int]] = (1 to 10).map(_ => eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.MILLISECONDS)

  def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = for {
    startTime <- currentTime
    effVal <- effect
    endTime <- currentTime
    _ <- console.putStrLn(s"Running time : ${endTime - startTime} ms")
  } yield effVal


  val sumEff = ZIO.collectAll(effects).map(col => col.foldLeft(0)((acc, el) => acc+el))

  lazy val app = printEffectRunningTime(sumEff).tap( v => console.putStrLn(s"Total sum is $v"))

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val sunEffPar = ZIO.collectAllPar(effects).map(col => col.foldLeft(0)((acc, el) => acc+el))
  lazy val appSpeedUp = printEffectRunningTime(sunEffPar).tap( v => console.putStrLn(s"Total sum is $v"))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  type PrintTimeService = Has[PrintTimeService.Service]

  @accessible
  object PrintTimeService {
    trait Service {
      def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A]
    }

    val live: ULayer[Has[Service]] = ZLayer.succeed(new Service {
      private val currentTime: URIO[Clock, Long] = clock.currentTime(TimeUnit.MILLISECONDS)
      override def printEffectRunningTime[R, E, A](effect: ZIO[R, E, A]): ZIO[Clock with Console with R, E, A] = for {
        startTime <- currentTime
        effVal <- effect
        endTime <- currentTime
        _ <- console.putStrLn(s"Running time : ${endTime - startTime} ms")
      } yield effVal
    })

    //boilerplate code which usually should be generated in compile time by accesible macros
    //Used for Intellij as it does not work with accessible macros
//    def printEffectRunningTime[R,E,A]( effect : ZIO[R,E,A]) : ZIO[PrintTimeService with Clock with Console with R, E, A] =
//      ZIO.accessM(_.get.printEffectRunningTime(effect))
  }


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg: ZIO[PrintTimeService with Clock with Console with random.Random, Nothing, Int] =
    PrintTimeService.printEffectRunningTime(sumEff).tap( v => console.putStrLn(s"Total sum is $v"))

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  lazy val runApp =
    appWithTimeLogg.provideSomeLayer[Console with Clock with random.Random](PrintTimeService.live)

}
