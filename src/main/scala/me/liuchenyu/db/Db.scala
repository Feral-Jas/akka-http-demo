package me.liuchenyu.db

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

class Db {
  final case class Greet(whom:String,replyTo:ActorRef[Greeted])
  final case class Greeted(whom:String,from:ActorRef[Greet])

  def apply():Behavior[Greet] = Behaviors.receive({
    (context,message)=>{
      context.log.info("i am {}"+ message.whom)
      message.replyTo ! Greeted(message.whom ,context.self)
      Behaviors.same
    }
  })
}
