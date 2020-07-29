import java.io.File

import zio.actors.Actor.Stateful
import zio.actors._
import zio.RIO
import zio.console._
import zio.duration.Duration

object Main extends zio.App {

  val configFile = Some(new File("./src/main/resources/application.conf"))


  sealed trait PingPong[+A]
  case class Ping(sender: ActorRef[PingPong]) extends PingPong[Unit]
  case object Pong extends PingPong[Unit]
  case class GameInit(recipient: ActorRef[PingPong]) extends PingPong[Unit]

  def run(args: List[String]) =
    program.fold(_ => 1, _ => 0)

  val protoHandler = new Stateful[Console, Unit, PingPong] {
    override def receive[A](state: Unit, msg: PingPong[A], context: Context): RIO[Console, (Unit, A)] =
      msg match {
        case Ping(sender) =>
          for {
            _ <- putStrLn("Ping!")
            _ <- sender ! Pong
          } yield ((), ())

        case Pong =>
          for {
            _ <- putStrLn("Pong!")
          } yield ((), ())

        case GameInit(to) =>
          for {
            self <- context.self[PingPong]
            _ <- to ! Ping(self)
          } yield ((), ())
      }
  }

  val program = for {
    actorSystemRoot <- ActorSystem("testSystemOne", configFile)
    one <- actorSystemRoot.make("actorOne", Supervisor.none, (), protoHandler)

    actorSystem <- ActorSystem("testSystemTwo", configFile)
    _ <- actorSystem.make("actorTwo", Supervisor.none, (), protoHandler)

    remoteActor <- actorSystemRoot.select[PingPong](
      "zio://testSystemTwo@127.0.0.1:9669/actorTwo"
    )

    _ <- one ! GameInit(remoteActor)
    _ <- zio.clock.sleep(Duration.Infinity)

  } yield ()

}